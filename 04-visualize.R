library(tidytransit)

# Pull WMATA rail --------------------------------------------------------------

after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
                        files = NULL, quiet = TRUE)

wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

# Before access scores ---------------------------------------------------------

before_med <- filter(before, percentile == 50)

jenks <- BAMMtools::getJenksBreaks(before_med$accessibility, 5, subset = NULL)

before_med$acc_jenks <- cut(before_med$accessibility, jenks, include.lowest = TRUE)
before_med$id <- as.numeric(before_med$id)

before_plot <- left_join(dc_hex, before_med, by = c("hexid" = "id"))

ggplot() + 
  geom_sf(data = before_plot, aes(color = acc_jenks, fill = acc_jenks)) + 
  scale_fill_viridis_d(direction = -1) + 
  scale_color_viridis_d(direction = -1) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  theme_bw()
