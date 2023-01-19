library(r5r)
library(sf)
library(dplyr)

options(java.parameters = '-Xmx12G')
Sys.setenv(TZ = 'America/New_York')

data_path <- "feeds_202211/"

# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

# Set parameters
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 5000
max_trip_duration <- 180

# 5-minute blocks between 9am and 6pm central
set.seed(732)
departure_datetimes <- 
  seq(as.POSIXct("11-09-2022 9:00:00", format = "%m-%d-%Y %H:%M:%S"), 
      as.POSIXct("11-09-2022 17:55:00", format = "%m-%d-%Y %H:%M:%S"), 
      by = "5 min") +
  runif(108, 0, 300)

attr(departure_datetimes, "tzone") <- "" # This shouldn't be necessary 

bg_pts <- 
  read.csv("data/block_group_pts.csv") %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant") %>%
  select(id = GEOID, geometry)

ttm_all_early <- full_tt_matrix(bg_pts, bg_pts, departure_datetimes)


# calculate a travel time matrix
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = points,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration,
                          verbose = FALSE)

head(ttm)