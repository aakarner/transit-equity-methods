library(sf)
library(dplyr)
library(ggplot2)
library(tidytransit)

# Pull WMATA rail --------------------------------------------------------------

# after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
#                        files = NULL, quiet = TRUE)

# wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

wmata_shapes <- st_read("data/Metro_Lines_Regional.geojson")

###

dc_scores <- 
  st_read("output/dc_scores.geojson") %>%
  st_transform("EPSG:4326") 

# Standardize scores for a needs-gap analysis 
dc_scores <-
  group_by(dc_scores, date) %>%
  mutate(std_score = scale(score),
         std_demand = 
           (scale(hhld_nocar) + scale(hhld_single_mother) + scale(pop_poverty)) / 3,
         gap = std_demand - std_score, # Large +ve means a gap, large -ve means oversupply
         categ = ifelse(std_demand > 0 & std_score > 0, "high-high",
                 ifelse(std_demand > 0 & std_score < 0, "high-low",
                 ifelse(std_demand < 0 & std_score < 0, "low-low",
                 ifelse(std_demand < 0 & std_score > 0, "low-high", NA))))
         )

range(dc_scores$std_score)
range(dc_scores$std_demand, na.rm = TRUE)

summarize(dc_scores, avg_score = mean(std_score), sd_score = sd(std_score))

# Histogram of scores
ggplot() + 
  geom_histogram(data = dc_scores, aes(log(score)))

ggplot() + 
  geom_histogram(data = dc_scores, aes(demand)) 

# Basic regional map
ggplot() + 
  geom_sf(data = dc_scores)


# Basic choropleths of supply and demand centered on areas with rail service
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

# Is the "gap" being driven by a high demand or low supply?
ggplot() + 
  geom_point(data = filter(dc_scores, score > 500000),
             aes(x = gap, y = std_score)) + 
  theme_bw()

ggplot() + 
  geom_point(data = dc_scores, aes(x = gap, y = std_demand)) + 
  theme_bw()

ggplot() + 
  geom_point(data = dc_scores, aes(x = std_score, y = std_demand)) + 
  theme_bw() + 
  xlab("standardized access score (supply)") + 
  ylab("standardized demand score")

# Where are gaps
ggplot() + 
  geom_sf(data = filter(dc_scores, score > 0), aes(color = gap, fill = gap)) +
  geom_sf(data = wmata_shapes, color = "black") + 
  facet_wrap(~ date) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  scale_fill_viridis_c() + 
  scale_color_viridis_c() + 
  ggthemes::theme_map()



ggplot() + 
  geom_sf(data = filter(dc_scores, score > 100000), aes(color = categ, fill = categ)) +
  geom_sf(data = wmata_shapes, color = "black") + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d() + 
  ggthemes::theme_map()

# population totals in the categories

dc_scores %>%
  filter(score > 100000) %>%
  st_drop_geometry() %>%
  group_by(categ) %>%
  summarize(totpop = sum(pop_total, na.rm = TRUE))


# Show that there are many places with large "gaps" that are on heavy rail
# Show that the relative nature of the measure is silly when there's a big service cut since
# the gaps won't change at all. Many people would be below the threshold but the 
# "desert" doesn't change.