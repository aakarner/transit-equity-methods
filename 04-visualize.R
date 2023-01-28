library(tidytransit)

# Pull WMATA rail --------------------------------------------------------------

after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
                        files = NULL, quiet = TRUE)

wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

# Prepare access scores --------------------------------------------------------

after_med <- filter(after, percentile == 50)
before_med <- filter(before, percentile == 50)



# Compare scores ---------------------------------------------------------------

compare_scores <- 
  inner_join(before_med, after_med, by = "id") %>%
  mutate(difference = accessibility.y - accessibility.x)

jenks <- BAMMtools::getJenksBreaks(compare_scores$difference, 5, subset = NULL)

compare_scores$diff_jenks <- 
  cut(compare_scores$difference, jenks, 
      labels = c("< -14,000", "-14,000 - 5,000", "5,000 - 25,000", "> 25,000"),
      include.lowest = TRUE)
compare_scores$id <- as.numeric(compare_scores$id)

compare_plot <- 
  left_join(dc_hex, compare_scores, by = c("hexid" = "id")) %>%
  filter(!is.na(diff_jenks)) %>%
  st_intersection(wmata_area)

# Comparing before/after
ggplot() + 
  geom_sf(data = compare_plot, aes(color = diff_jenks, fill = diff_jenks)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  # coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  scale_fill_viridis_d(direction = -1) + 
  scale_color_viridis_d(direction = -1) + 
  guides(fill = guide_legend(title = "access change after\nsilver line opening"),
         color = guide_legend(title = "access change after\nsilver line opening")) +
  ggthemes::theme_map()

ggsave("output/comparison.png")


# Before
ggplot() + 
  geom_sf(data = compare_plot, aes(color = accessibility.x, fill = accessibility.x)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  # coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  # scale_fill_viridis_d(direction = -1) + 
  # scale_color_viridis_d(direction = -1) + 
  guides(fill = guide_legend(title = "access score"),
         color = guide_legend(title = "access score")) +
  ggthemes::theme_map()

ggsave("output/before.png")

# After
ggplot() + 
  geom_sf(data = compare_plot, aes(color = accessibility.y, fill = accessibility.y)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  # coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  # scale_fill_viridis_d(direction = -1) + 
  # scale_color_viridis_d(direction = -1) + 
  guides(fill = guide_legend(title = "access score"),
         color = guide_legend(title = "access score")) +
  ggthemes::theme_map()

ggsave("output/after.png")
