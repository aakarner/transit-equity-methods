library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytransit)
library(tigris)
library(tidycensus)

# Pull WMATA rail --------------------------------------------------------------

# after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
#                         files = NULL, quiet = TRUE)

# wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

wmata_shapes <- st_read("data/Metro_Lines_Regional.geojson")

# Demographic map
dc_to_map <- 
  dc_scores %>%
  mutate(share_black = pop_black / pop_total,
         share_white = pop_white / pop_total)

ggplot(dc_to_map) + 
  geom_sf(aes(col = share_black, fill = share_black))


###

dc_scores <- 
  st_read("output/dc_scores.geojson") %>%
  st_transform("+init=EPSG:4326")

urban_def <- read.csv("data/urban.csv")

dc_scores %>%
  st_drop_geometry() %>%
  filter(GEOID %in% urban_def$bg_id) %>%
  group_by(date) %>%
  summarize(
    white_wtd = 
      sum(score * pop_white, na.rm = TRUE) / sum(pop_white, na.rm = TRUE), 
    black_wtd = 
      sum(score * pop_black, na.rm = TRUE) / sum(pop_black, na.rm = TRUE))


dc_scores %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarize(overall_mean = mean(score, na.rm = TRUE), 
            overall_med = median(score, na.rm = TRUE))


# Standardize scores for a needs-gap analysis 
dc_scores <-
  dc_scores %>%
  filter(GEOID %in% urban_def$bg_id) %>%
  group_by(date) %>%
  mutate(std_score = scale(score),
         std_demand1 = scale(pop_poverty),
         std_demand2 = 
           (scale(hhld_nocar) + scale(hhld_single_mother) + scale(pop_poverty)) / 3,
         gap1 = std_demand1 - std_score,
         gap2 = std_demand2 - std_score, # Large +ve means a gap, large -ve means oversupply
         categ = ifelse(std_demand1 > 0 & std_score > 0, "high-high",
                 ifelse(std_demand1 > 0 & std_score < 0, "high-low",
                 ifelse(std_demand1 < 0 & std_score < 0, "low-low",
                 ifelse(std_demand1 < 0 & std_score > 0, "low-high", NA)))),
         )



dc_scores %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  group_by(categ) %>% 
  summarize(pop_total = sum(pop_total, na.rm = TRUE),
            pov_pop = sum(pop_poverty, na.rm = TRUE),
            desert = sum(gap1 > 0, na.rm = TRUE),
            not_desert = sum(gap1 <= 0, na.rm = TRUE))

dc_scores %>%
  st_drop_geometry() %>%
  pivot_longer(cols = std_score:std_demand2,
               names_to = "var", 
               values_to = "value") %>%
  filter(!is.na(categ)) %>%
  ggplot() + 
    geom_density(aes(value, color = var, fill = var), alpha = 0.5) + 
    theme_bw()

range(dc_scores$std_score)
range(dc_scores$std_demand1, na.rm = TRUE)
range(dc_scores$std_demand2, na.rm = TRUE)


summarize(dc_scores, avg_score = mean(std_score), sd_score1 = sd(std_score))

# We can correct the scale using a different normalization approach


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
  geom_sf(data = dc_scores, aes(color = std_demand1, fill = std_demand1)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_viridis_c(direction = -1) + 
  scale_color_viridis_c(direction = -1) + 
  ggthemes::theme_map()

ggplot() +
  geom_sf(data = dc_scores, aes(color = std_score, fill = std_score)) + 
  geom_sf(data = wmata_shapes, color = "black") + 
  geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_viridis_c(direction = -1) + 
  scale_color_viridis_c(direction = -1) + 
  ggthemes::theme_map()


# Is the "gap" being driven by a high demand or low supply?
ggplot() + 
  geom_point(data = filter(dc_scores, !is.na(categ)),
             aes(x = gap1, y = std_score, color = categ)) + 
  scale_color_viridis_d() + 
  xlab("\"gap\" (demand - supply)") + 
  ylab("standardized access score") + 
  theme_bw()

# What are the access conditions faced by people in poverty? 
# This will help us establish a sufficiency threshold. 
ggplot() + 
  geom_histogram(data = dc_scores, 
                 aes(std_score, weight = pop_poverty, color = categ, fill = categ)) + 
  facet_wrap(~(gap1 > 0)) + 
  theme_bw()




ggplot() + 
  geom_histogram(data = dc_scores, aes(std_score))


ggplot() + 
  geom_point(data = dc_scores, aes(x = gap, y = std_demand)) + 
  theme_bw()

ggplot() + 
  geom_point(data = dc_scores, aes(x = std_score, y = std_demand)) + 
  theme_bw() + 
  xlab("standardized access score (supply)") + 
  ylab("standardized demand score")

# Where are gaps
# ggplot() + 
#   geom_sf(data = filter(dc_scores, score > 0), aes(color = gap, fill = gap)) +
#   geom_sf(data = wmata_shapes, color = "black") + 
#   facet_wrap(~ date) + 
#   coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
#   scale_fill_viridis_c() + 
#   scale_color_viridis_c() + 
#   ggthemes::theme_map()

ggplot() + 
  geom_sf(data = dc_scores, aes(fill = gap1 > 0), color = NA) +
  geom_sf(data = wmata_shapes, color = "white") +
  facet_wrap(~ date) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  scale_fill_manual(values = c("#5FA052", "#D7504D")) + 
  ggthemes::theme_map()

ggsave("output/basicDeserts.png")

ggplot() + 
  geom_sf(data = filter(dc_scores, !is.na(gap1)), aes(color = categ, fill = categ)) +
  geom_sf(data = wmata_shapes, color = "black") + 
  geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  facet_wrap(~(gap1 > 0) + date) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d() + 
  ggthemes::theme_map()

ggsave("output/gapTypes.png")

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