library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lehdr)

# DC uses the Maryland State Plane NAD 83 coordinate system
# https://octo.dc.gov/page/coordinate-system-standards
# EPSG: 2248 
# https://epsg.io/2248

# The TransitCenter Equity Dashboard uses data from the 2018 five-year ACS

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
# Montgomery and Prince George's counties in Maryland. 
# Source: https://www.arlingtonva.us/files/sharedassets/public/budget/documents/fy22-p-30-metro-02.20.21.pdf
# Extension to Dulles adds Loudoun county, Virginia, as well. 

# State FIPS codes:
# DC: 11
# MD: 24
# VA: 51

wmata_area <- 
  counties(state = c("DC", "MD", "VA")) %>%
  filter(NAMELSAD %in% c("District of Columbia",
                         "Arlington County", "Fairfax County", "Loudoun County",
                         "Alexandria city", "Fairfax city", "Falls Church city", 
                         "Prince George's County") | 
         NAMELSAD == "Montgomery County" & STATEFP == 24) %>% # don't pull VA here
  st_union() %>%
  st_transform("EPSG:2248")

wmata_states <- 
  states() %>% 
  filter(
    NAME %in% c("Virginia", "Maryland", "District of Columbia"))

ggplot() + 
  # geom_sf(data = wmata_states, color = "black", fill = NA) + 
  geom_sf(data = dc_cbsa, color = "black", fill = NA) + 
  geom_sf(data = wmata_area, fill = "green") + 
  ggthemes::theme_map()

# Pull ACS data and interpolate to the grid cell level -------------------------
v18 <- load_variables(2018, "acs5", cache = TRUE)

View(v18)

demog_vars <- c(
  "B01001_001", # total population
  "B23025_001", # working-age population employed and not
  "B25046_001") # aggregate vehicles available

hl_vars <- 
  c("B03002_003", # white alone
    "B03002_004", # black alone
    "B03002_006", # Asian
    "B03002_012") # Hispanic or Latino

demographics <- 
  get_acs(geography = "block group",
          variables = demog_vars,
          state = c("DC", "MD", "VA", "WV"),
          geometry = TRUE,
          output = "wide",
          year = 2018) %>%
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

# Regional maps ----------------------------------------------------------------
demographics_toPlot <- 
  get_acs(geography = "block group",
          variables = hl_vars,
          summary_var = "B03002_001",
          state = c("DC", "MD", "VA", "WV"),
          geometry = TRUE,
          year = 2018) %>%
  # st_transform("EPSG:2248") %>%
  mutate(orig_area = units::drop_units(st_area(.)),
         pop_share = estimate / summary_est)

ggplot() + 
  geom_sf(data = demographics_toPlot, aes(color = pop_share, fill = pop_share)) + 
  geom_sf(data = wmata_states, col = "white", fill = NA) + 
  facet_wrap(~ variable) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c() +
  ggthemes::theme_map()
  

# "Transit supply" -------------------------------------------------------------

# DC sidewalks: https://opendata.dc.gov/datasets/2347fa1f3fd9412dbf11aa6441ddca8b_83/about
# These are represented as polygons
# DC roadway sub-blocks (with bike infrastructure data): 
# https://opendata.dc.gov/datasets/df571ab7fea446e396bf2862d0ab6833_162/explore?location=38.894927%2C-77.015000%2C12.73

# Issue with reading a shapefile when there's inconsistent elevation information present
# https://github.com/r-spatial/sf/issues/2081
gdal_utils(util = "vectortranslate",
           source = "D:/Dropbox/Work/transit-equity-pitfalls/data/Roadway_SubBlock.geojson",
           destination = "D:/Dropbox/Work/transit-equity-pitfalls/data/Roadway_SubBlock_XY.geojson",
           options = c("-dim", "XY"))

dc_roads <- st_read("D:/Dropbox/Work/transit-equity-pitfalls/data/Roadway_SubBlock_XY.geojson")
dc_sidewalks <- st_read("D:/Dropbox/Work/transit-equity-pitfalls/data/sidewalks.geojson")

ggplot() + 
  geom_sf(data = filter(dc_sidewalks, OBJECTID %in% 1))

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