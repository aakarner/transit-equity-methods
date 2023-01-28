library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(lehdr)

# DC uses the Maryland State Plane NAD 83 coordinate system
# https://octo.dc.gov/page/coordinate-system-standards
# EPSG: 2248 
# https://epsg.io/2248

# Pull spatial data and create hexagon grid ------------------------------------

dc_cbsa <- 
  core_based_statistical_areas() %>%
  filter(grepl("Washington-Arlington", NAME)) %>%
  st_transform("EPSG:2248")

# 1640 is the desired size. Set larger as needed to make computations tractable.
dc_hex <- 
  st_make_grid(dc_cbsa, cellsize = 1640, square = FALSE) %>%
  st_sf() %>%
  st_intersection(dc_cbsa) %>%
  mutate(hexid = as.numeric(row.names(.)))
  
# dc_hex$area <- st_area(dc_hex)
# Area of each gridcell is 2,329,262 ft^2

# WAMA service area: Arlington and Fairfax counties, the cities of Alexandria, 
# Fairfax, and Falls Church in Virginia, the District of Columbia, and 
# Montgomery and Prince George’s counties in Maryland. 
# Source: https://www.arlingtonva.us/files/sharedassets/public/budget/documents/fy22-p-30-metro-02.20.21.pdf

# State FIPS codes:
# DC: 11
# MD: 24
# VA: 51

wmata_area <- 
  counties(state = c("DC", "MD", "VA")) %>%
  filter(NAMELSAD %in% c("District of Columbia",
                         "Arlington County", "Fairfax County", "Alexandria city", 
                         "Fairfax city", "Falls Church city", 
                         "Prince George's County") | 
         NAMELSAD == "Montgomery County" & STATEFP == 24) %>% # don't pull VA here
  st_union()

wmata_states <- states() %>% filter(NAME %in% c("Virginia", "Maryland"))

ggplot() + 
  # geom_sf(data = wmata_states, color = "black", fill = NA) + 
  geom_sf(data = dc_cbsa, color = "black", fill = NA) + 
  geom_sf(data = wmata_area, fill = "green") + 
  ggthemes::theme_map()

# Pull ACS data and interpolate to the grid cell level -------------------------
demographics <- 
  get_acs(geography = "block group",
          variables = "B01001_001",
          state = c("DC", "MD", "VA", "WV"),
          geometry = TRUE,
          year = 2021) %>%
  st_transform("EPSG:2248") %>%
  mutate(orig_area = units::drop_units(st_area(.)))

# ggplot() + 
#   geom_sf(data = dc_jobs_bg) + 
#   # geom_sf(data = demographics) + 
#   geom_sf(data = dc_cbsa, color = "red", fill = NA)

hex_demogs <- 
  st_intersection(demographics, dc_hex) %>%
  mutate(new_area = units::drop_units(st_area(.))) %>%
  mutate(hex_count = estimate * new_area / orig_area) %>%
  group_by(hexid) %>%
  summarize(population = sum(hex_count))

sum(hex_demogs$population) # 6,332,055

# Pull LODES data and interpolate to the grid cell level------------------------

dc_lodes <- 
  grab_lodes(
    state = c("dc", "wv", "va", "md"), year = 2019, lodes_type = "wac", 
    job_type = "JT01", segment = "S000", state_part = "main", 
    agg_geo = "bg")

dc_bgs <- 
  rbind(
    block_groups(state = "DC", year = 2019),
    block_groups(state = "WV", year = 2019),
    block_groups(state = "VA", year = 2019),
    block_groups(state = "MD", year = 2019))

dc_jobs_bg <- 
  left_join(dc_bgs, dc_lodes, by = c("GEOID" = "w_bg")) %>%
  st_transform("EPSG:2248") %>%
  mutate(orig_area = units::drop_units(st_area(.)))

hex_jobs <- 
  st_intersection(dc_jobs_bg, dc_hex) %>%
  mutate(new_area = units::drop_units(st_area(.))) %>%
  mutate(hex_jobs = C000 * new_area / orig_area) %>%
  group_by(hexid) %>%
  summarize(jobs = sum(hex_jobs))

# Merge demographic and jobs data ----------------------------------------------

# Slight discrepancy in total hex counts is due to how features in water 
# are treated. Take the ACS demographic gridcells as the base. 

hex_final <- 
  left_join(hex_demogs, st_drop_geometry(hex_jobs), by = "hexid") %>%
  relocate(jobs, .after = population)

hex_points <- 
  st_centroid(hex_final) %>%
  st_transform("EPSG:4326") 

hex_coords <- as.data.frame(st_coordinates(hex_points))
names(hex_coords) <- c("lon", "lat")
hex_coords <- cbind(st_drop_geometry(hex_points), hex_coords)
names(hex_coords)[1] <- "id"