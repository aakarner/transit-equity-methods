library(sf)
library(dplyr)
library(ggplot2)
library(tidytransit)

# Pull WMATA rail --------------------------------------------------------------

after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
                        files = NULL, quiet = TRUE)

wmata_shapes <- shapes_as_sf(after_gtfs$shapes)


###

dc_scores <- 
  st_read("output/dc_scores.geojson") %>%
  st_transform("EPSG:4326") 

dc_scores <-
  group_by(dc_scores, date) %>%
  mutate(std_score = scale(score),
         std_demand = 
           (scale(hhld_nocar) + scale(hhld_single_mother) + scale(pop_poverty)) / 3
         )

summarize(dc_scores, avg_score = mean(std_score), sd_score = sd(std_score))


ggplot() + 
  geom_sf(data = dc_scores, aes(color = std_demand, fill = std_demand)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_viridis_c(direction = -1) + 
  scale_color_viridis_c(direction = -1)

ggplot() +
  geom_sf(data = dc_scores, aes(color = std_score, fill = std_score)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE)



# Show that there are many places with large "gaps" that are on heavy rail
# Show that the relative nature of the measure is silly when there's a big service cut
# Many people are now below the threshold but the "desert" doesn't change 