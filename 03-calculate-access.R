options(java.parameters = '-Xmx32G')
Sys.setenv(TZ = 'America/New_York')
# Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-11')

library(r5r)
library(sf)
library(dplyr)
library(lehdr)

set.seed(732)

# Set parameters
mode <- c("WALK", "TRANSIT")
max_walk_time <- 60
max_trip_duration <- 180

## Scores prior to Silver line opening -----------------------------------------

data_path <- "feeds_202211"

# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

departure_datetime = as.POSIXct("11-09-2022 9:00:00", format = "%m-%d-%Y %H:%M:%S")

before <- 
  accessibility(
    r5r_core,
    origins = hex_coords,
    destinations = hex_coords,
    opportunities_colnames = "jobs",
    mode = mode,
    departure_datetime = departure_datetime,
    time_window = 120,
    percentiles = c(50, 75, 90, 95),
    decay_function = "step",
    cutoffs = 45,
    max_walk_time = max_walk_time,
    max_trip_duration = max_trip_duration,
    n_threads = 12,
    progress = TRUE
  )

stop_r5()

## Scores after silver line opening --------------------------------------------

data_path <- "feeds_202301"

# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

departure_datetime = as.POSIXct("01-11-2023 9:00:00", format = "%m-%d-%Y %H:%M:%S")

after <- 
  accessibility(
    r5r_core,
    origins = hex_coords,
    destinations = hex_coords,
    opportunities_colnames = "jobs",
    mode = mode,
    departure_datetime = departure_datetime,
    time_window = 120,
    percentiles = c(50, 75, 90, 95),
    decay_function = "step",
    cutoffs = 45,
    max_walk_time = max_walk_time,
    max_trip_duration = max_trip_duration,
    n_threads = 12,
    progress = TRUE
  )

stop_r5()

## Combine results and visualize -----------------------------------------------