library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(ggplot2)

# DC uses the Maryland State Plane NAD 83 coordinate system
# https://octo.dc.gov/page/coordinate-system-standards
# EPSG: 2248 
# https://epsg.io/2248

# Pull spatial data and create hexagon grid

dc_cbsa <- 
  core_based_statistical_areas() %>%
  filter(grepl("Washington-Arlington", NAME)) %>%
  st_transform("EPSG:2248")

# 1640 is the desired size. Set to larger for now to make computations tractable.
dc_hex <- 
  st_make_grid(dc_cbsa, cellsize = 7500, square = FALSE) %>%
  st_sf() %>%
  st_intersection(dc_cbsa) %>%
  mutate(hexid = as.numeric(row.names(.)))
  
# dc_hex$area <- st_area(dc_hex)
# Area of each gridcell is 2,329,262 ft^2

# Pull ACS data and interpolate to the grid cell level
demographics <- 
  get_acs(geography = "block group",
          variables = "B01001_001",
          state = c("DC", "MD", "VA", "WV"),
          geometry = TRUE,
          year = 2021) %>%
  st_transform("EPSG:2248") %>%
  mutate(orig_area = units::drop_units(st_area(.)))

hex_demogs <- 
  st_intersection(demographics, dc_hex) %>%
  mutate(new_area = units::drop_units(st_area(.))) %>%
  mutate(hex_count = estimate * new_area / orig_area) %>%
  group_by(hexid) %>%
  summarize(population = sum(hex_count))

sum(hex_demogs$population) # 6,332,055