library(tigris)
library(tidycensus)
library(ggplot2)
library(tidytransit)
library(sf)
library(dplyr)
library(httr)
library(lubridate)
library(jsonlite)

# Getting started with httr: 
# https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html

key <- readLines("transitland.key")

# Pull all feeds  --------------------------------------------------------------
# Before date: November 9, 2022
# After date: January 16, 2023

before_date <- as_date("2022-11-09")

for (i in dc_gtfs_list$transit_land_id) {

  feed_versions <- 
    GET("https://transit.land/api/v2/rest/feed_versions",
        query = 
          list(apikey = key,
               feed_onestop_id = i,
               limit = 100))

  json_response <- fromJSON(rawToChar(feed_versions$content))$feed_versions
  json_response$the_date <- as_datetime(json_response$fetched_at)

  feed_idx <- 
    tryCatch(
      idx <- max(which((before_date < json_response$the_date) == TRUE)) + 1,
      warning = function(cond) {
        message(paste("feed caused a warning:", i))
              message("Here's the original warning message:")
              message(cond)
        return(1)
      })
  
  this_feed <- json_response$sha1[feed_idx]

  download_zip_before <-
   GET(paste0(
     "https://transit.land/api/v2/rest/feed_versions/",
     this_feed,
     "/download"),
     query = list(apikey = key))
  
  bin <- content(download_zip_before, "raw")
  writeBin(bin, paste0("feeds_202211/", this_feed, ".zip"))

  print(paste0(i, " ", this_feed))
  
}
  
# Pull all "current" feeds -----------------------------------------------------
# Date: January 16, 2023

for (i in dc_gtfs_list$transit_land_id) {
  
  download_zip_after <- 
    GET(paste0(
      "https://transit.land/api/v2/rest/feeds/", 
      i, 
      "/download_latest_feed_version"),
      query = list(apikey = key))
  
  bin_after <- content(download_zip_after, "raw")
  writeBin(bin_after, paste0("feeds_202301/", i, ".zip"))
  
}
