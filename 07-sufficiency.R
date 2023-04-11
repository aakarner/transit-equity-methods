library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyr)
library(tidytransit)
library(tigris)
library(tidycensus)

# Pull WMATA rail --------------------------------------------------------------

# after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
#                         files = NULL, quiet = TRUE)

# wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

wmata_shapes <- st_read("data/Metro_Lines_Regional.geojson")

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
dc_scores_final <-
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
         below_med = ifelse(score < 138169, 1, 0),
         `desert status` = ifelse(gap1 > 0, "desert", "not")
         ) %>%
  st_transform("EPSG:2248")

dc_scores_final %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  group_by(categ, date) %>% 
  summarize(pop_total = sum(pop_total, na.rm = TRUE),
            pov_pop = sum(pop_poverty, na.rm = TRUE),
            desert = sum(gap1 > 0, na.rm = TRUE),
            not_desert = sum(gap1 <= 0, na.rm = TRUE)) %>%
  arrange(date, categ)

# How many people are in deserts before and after the covid cut? 
dc_scores_final %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  group_by(date, `desert status`) %>% 
  summarize(pop_total = sum(pop_total, na.rm = TRUE),
            pov_pop = sum(pop_poverty, na.rm = TRUE),
            count = n()) %>%
  arrange(date, `desert status`)

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

# Demographic map --------------------------------------------------------------
dc_to_map <- 
  dc_scores %>%
  mutate(share_black = pop_black / pop_total,
         share_white = pop_white / pop_total)

ggplot(dc_to_map) + 
  geom_sf(aes(col = share_black, fill = share_black))


# Histogram of scores
ggplot() + 
  geom_histogram(data = dc_scores, aes(log(score)))

ggplot() + 
  geom_histogram(data = dc_scores, aes(demand)) 

# Basic regional map -----------------------------------------------------------
ggplot() + 
  geom_sf(data = dc_scores)


# Basic choropleths of supply and demand centered on areas with rail service -----
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


# Is the "gap" being driven by a high demand or low supply? --------------------
ggplot() + 
  geom_point(data = filter(dc_scores_final, !is.na(categ)),
             aes(x = gap1, y = std_score, color = categ)) + 
  scale_color_viridis_d() + 
  xlab("\"gap\" (demand - supply)") + 
  ylab("standardized access score") + 
  facet_wrap(~ date) + 
  guides(color = guide_legend(title = "demand-supply")) +
  theme_bw() + 
  theme(legend.position = "bottom")
  
  # theme(legend.position = c(0.9, 0.8),
  #       legend.background = element_blank(),
  #       legend.box.background = element_rect(colour = "black"))

ggsave("figures/supplyGapScatter.png", width = 7.5, height = 4)

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

# Where are gaps ---------------------------------------------------------------
# ggplot() + 
#   geom_sf(data = filter(dc_scores, score > 0), aes(color = gap, fill = gap)) +
#   geom_sf(data = wmata_shapes, color = "black") + 
#   facet_wrap(~ date) + 
#   coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
#   scale_fill_viridis_c() + 
#   scale_color_viridis_c() + 
#   ggthemes::theme_map()

# Where are deserts? -----------------------------------------------------------

scale_params <- tibble::tibble(
  date = c("June 2020"),
  width_hint = 0.25,
  style = c("bar"),
  location = c("br"),
  unit_category = c("imperial"),
  text_col = c("black"),
  line_col = c("black")
)

ggplot() + 
  geom_sf(data = dc_scores_final, aes(fill = `desert status`), color = NA) +
  geom_sf(data = wmata_shapes, color = "white") +
  geom_sf(data = wmata_states, fill = NA, col = "black") + 
  facet_wrap(~ date) + 
  # coord_sf(xlim = c(-77.3, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  coord_sf(xlim = c(1226715.965140128, 1369011.5944263502), ylim = c(376465.63918774325, 540355.250319933)) + 
  scale_fill_manual(values = c("#D7504D", "#5FA052")) +
  ggthemes::theme_map() + 
  theme(panel.background = element_rect(fill = grey(0.9))) + 
  annotation_scale(
    aes(width_hint = width_hint, 
        style = style, 
        location = location, 
        unit_category = unit_category,
        text_col = text_col, 
        line_col = line_col),
    data = scale_params,
    plot_unit = "ft") + 
  annotation_north_arrow(style = north_arrow_minimal())

ggsave("figures/basicDeserts.png", width = 9, height = 4.5)

ggplot() + 
  geom_sf(data = filter(dc_scores_final, !is.na(gap1)), aes(color = categ, fill = categ)) +
  geom_sf(data = wmata_shapes, color = "white") + 
  geom_sf(data = wmata_states, color = "black", fill = NA) + 
  facet_wrap(~`desert status` + date) + 
  # coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
  coord_sf(xlim = c(1226715.965140128, 1369011.5944263502), ylim = c(376465.63918774325, 540355.250319933)) + 
  scale_fill_viridis_d() + 
  scale_color_viridis_d() + 
  ggthemes::theme_map() + 
  theme(panel.background = element_rect(fill = grey(0.9)))

ggsave("output/gapTypes.png")

# population totals in the categories ------------------------------------------

lollipop <- 
  dc_scores_final %>%
  # filter(score > 100000) %>%
  st_drop_geometry() %>%
  group_by(categ, date, `desert status`) %>%
  summarize(totpop = sum(pop_total, na.rm = TRUE),
            num_bgs = n(),
            med_score = median(score)) %>%
  arrange(categ, date, `desert status`)

ggplot(lollipop) +
  geom_point(aes(x = totpop, y = categ, color = `desert status`))


ggplot(lollipop) + 
  geom_bar(aes(color = categ, fill = categ, x = totpop, y = `desert status`), stat = "identity") +
  facet_wrap(~ date) + 
  scale_fill_viridis_d() +
  scale_color_viridis_d()

# Sufficiency analysis ---------------------------------------------------------

auto_access <-
    inner_join(dc_bgs,
      read.csv("data/auto_accessibility.csv",
               colClasses = c("GEOID" = "character")))


ggplot() + 
  geom_sf(data = auto_access, aes(color = C000_P_c45_AM, fill = C000_P_c45_AM))

# Distribution of transit access
quantile(filter(dc_scores_final, date == "Feb. 2020")$score, c(seq(0.1, 0.5, 0.1)))

sufficiency <- 
  dc_scores_final %>%
  mutate(acc_quant = 
           cut(score, 
               breaks = c(0, 24694.55,  42947.60,  65159.75,  95754.40, 138169.00, 1e6)))

suff_table <- 
  sufficiency %>%
    st_drop_geometry() %>%
    group_by(acc_quant, date) %>%
    summarize(total_pop = sum(pop_total),
              black_pop = sum(pop_black),
              white_pop = sum(pop_white),
              asian_pop = sum(pop_asiapacific),
              latinx_pop = sum(pop_hispanic),
              indig_pop = sum(pop_indig),
              pov_pop = sum(pop_poverty)) %>%
  arrange(date, acc_quant)
