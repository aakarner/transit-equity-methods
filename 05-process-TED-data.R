library(sf)
library(dplyr)
library(tidycensus)

dc_bgs <- 
  st_read("data/dc_bg.geojson") %>%
  st_transform("EPSG:2248")

# Pulled these from the map pages on the TED
scores <- 
  rbind(
    mutate(read.csv("data/ted_dc_C000_P_c45_AM_autoN_fareN_2020-02-23.csv",
                    colClasses = c("character", "numeric")),
           date = "Feb. 2020"),
    mutate(read.csv("data/ted_dc_C000_P_c45_AM_autoN_fareN_2020-06-21.csv",
                    colClasses = c("character", "numeric")),
           date = "June 2020"))

demogs <- read.csv("data/ted_population_dc_all.csv", 
                   colClasses = c("character", rep("numeric", 14)))


# Include income
med_income <- 
  get_acs(geography = "block group",
          variables = "B19013_001",
          state = c("DC", "MD", "VA", "WV"),
          geometry = FALSE,
          output = "wide",
          year = 2018)

demogs <- 
  left_join(demogs, 
            select(med_income, GEOID, med_inc = B19013_001E),
            by = c("block_group" = "GEOID"))

dc_scores <-
  inner_join(
    dc_bgs,
    scores,
    by = c("GEOID" = "block_group")
  ) %>%
  left_join(demogs, by = c("GEOID" = "block_group"))

st_write(dc_scores, "output/dc_scores.geojson", append = FALSE)
