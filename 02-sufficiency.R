library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyr)
library(tidytransit)
library(tigris)
library(tidycensus)
library(patchwork)

# Pull WMATA rail --------------------------------------------------------------

# after_gtfs <- read_gtfs("feeds_202301/f-dqc-wmata~rail.zip", 
#                         files = NULL, quiet = TRUE)

# wmata_shapes <- shapes_as_sf(after_gtfs$shapes)

wmata_shapes <- st_read("data/Metro_Lines_Regional.geojson")
wmata_states <- 
  states() %>% 
  filter(
    NAME %in% c("Virginia", "Maryland", "District of Columbia"))

dc_bgs <- 
  rbind(
    block_groups(state = "DC", year = 2019),
    block_groups(state = "WV", year = 2019),
    block_groups(state = "VA", year = 2019),
    block_groups(state = "MD", year = 2019))

###

dc_scores <- 
  st_read("data/dc_scores.geojson") %>%
  st_transform("+init=EPSG:4326") %>%
  mutate(scenario = factor(ifelse(date == 'Feb. 2020', "Before", "After"),
                           levels = c("Before", "After"),
                           labels = c("Before", "After")))

urban_def <- read.csv("data/urban.csv")

deciles  <- Hmisc::wtd.quantile(dc_scores$med_inc, weights=dc_scores$pop_total, 
                                probs=c( seq(0 , 1 , 0.1) ), 
                                type=c('quantile'), 
                                normwt=FALSE, na.rm=T)
dc_scores <- 
  dc_scores %>%
  mutate(deciles = findInterval(med_inc , deciles[ -length(deciles)]),
         dec_inc = factor(deciles, 
                          levels = 10:1,
                          labels = c('D10\nWealthiest', 9:2, 'D1\nPoorest')))

# Sanity checks
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
  group_by(scenario) %>%
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

# st_area(dc_scores_final)

# Sanity check
dc_scores_final %>% 
  ungroup() %>%
  st_drop_geometry() %>% 
  group_by(categ, date) %>% 
  summarize(pop_total = sum(pop_total, na.rm = TRUE),
            pov_pop = sum(pop_poverty, na.rm = TRUE),
            desert = sum(gap1 > 0, na.rm = TRUE),
            not_desert = sum(gap1 <= 0, na.rm = TRUE)) %>%
  arrange(date, categ)

# How many people are in deserts before and after the covid cut? 
desert_counts <- 
  dc_scores_final %>% 
    ungroup() %>% 
    mutate(sq_km = units::drop_units(units::set_units(st_area(.), km^2))) %>%
    st_drop_geometry() %>% 
    group_by(scenario, `desert status`) %>% 
    summarize(pop_total = sum(pop_total, na.rm = TRUE),
              pov_pop = sum(pop_poverty, na.rm = TRUE),
              tot_area = sum(sq_km),
              count = n()) %>%
    arrange(scenario, `desert status`) %>%
  pivot_longer(cols = pop_total:count) %>%
  mutate(varnames = factor(name, 
                       levels = c("count", "pop_total", "pov_pop", "tot_area"),
                       labels = c("block group count", "total population", "population in poverty", "total area (km\u00B2)")))

# Population totals
pop_figs <- 
  ggplot() + 
    geom_col(data = filter(desert_counts, grepl("population", varnames)),
             aes(y = value, x = `desert status`, fill = scenario),
             position = "dodge") +
    facet_wrap(~ varnames) + 
    xlab(NULL) + 
    scale_y_continuous(name = "count", labels = scales::comma) + 
    scale_fill_manual(values = c("#66A182", "#2E4057"), guide = "none") +
    theme_minimal() + 
    theme(legend.position = "bottom")


# Counts and area
count_figs <- 
  ggplot() + 
    geom_col(data = filter(desert_counts, !grepl("population", varnames)),
             aes(y = value, x = `desert status`, fill = scenario),
             position = "dodge") +
    facet_wrap(~ varnames) + 
    ylab(NULL) + 
    scale_fill_manual(values = c("#66A182", "#2E4057")) +
    theme_minimal() + 
    theme(legend.position = "bottom")

desert_stats <- pop_figs / count_figs + plot_annotation(tag_levels = 'A')
desert_stats
  
ggsave(desert_stats, filename = "figures/desert_stats.png", width = 9, height = 6, units = "in", dpi = 200)

# dc_scores_final %>%
#   st_drop_geometry() %>%
#   pivot_longer(cols = std_score:std_demand2,
#                names_to = "var", 
#                values_to = "value") %>%
#   filter(!is.na(categ)) %>%
#   ggplot() + 
#     geom_density(aes(value, color = var, fill = var), alpha = 0.5) + 
#     theme_bw()
# 
# range(dc_scores_final$std_score)
# range(dc_scores_final$std_demand1, na.rm = TRUE)
# range(dc_scores_final$std_demand2, na.rm = TRUE)
# summarize(dc_scores_final, avg_score = mean(std_score), sd_score1 = sd(std_score))

# Gaps analysis standardizing by the mean and sd that obtain in feb 2020

dc_scores_final %>%
  st_drop_geometry() %>%
  filter(GEOID %in% urban_def$bg_id) %>%
  group_by(date) %>%
  summarize(mean_score = mean(score),
            sd_score = sd(score))

# Standardize using the mean and sd from t1
dc_scores_alt <-
  dc_scores %>%
  filter(GEOID %in% urban_def$bg_id) %>%
  group_by(date) %>%
  mutate(std_score = scale(score, center = 246403, scale = 244616),
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

# How many people are in deserts before and after the covid cut using the
# mean and sd from t1? 
# Results not shown on manuscript p. 20
dc_scores_alt %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  group_by(date, `desert status`) %>% 
  summarize(pop_total = sum(pop_total, na.rm = TRUE),
            pov_pop = sum(pop_poverty, na.rm = TRUE),
            count = n()) %>%
  arrange(date, `desert status`)


# Basic choropleths of supply and demand centered on areas with rail service ---
# ggplot() + 
#   geom_sf(data = dc_scores_final, aes(color = std_demand1, fill = std_demand1)) + 
#   geom_sf(data = wmata_shapes, color = "black") + 
#   geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
#   coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
#   scale_fill_viridis_c(direction = -1) + 
#   scale_color_viridis_c(direction = -1) + 
#   ggthemes::theme_map()
# 
# ggplot() +
#   geom_sf(data = dc_scores_final, aes(color = std_score, fill = std_score)) + 
#   geom_sf(data = wmata_shapes, color = "black") + 
#   geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
#   coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
#   scale_fill_viridis_c(direction = -1) + 
#   scale_color_viridis_c(direction = -1) + 
#   ggthemes::theme_map()


# Is the "gap" being driven by a high demand or low supply? --------------------
# ggplot() + 
#   geom_hline(yintercept = 0, color = grey(0.8)) + 
#   geom_vline(xintercept = 0, color = grey(0.8)) +
#   geom_point(data = filter(dc_scores_final, !is.na(categ)),
#              aes(x = gap1, y = std_score, color = categ), alpha = 0.6, shape = 16, stroke = 0, size = 2) + 
#   scale_color_viridis_d() + 
#   xlab("\"gap\" (demand - supply)") + 
#   ylab("standardized access score") + 
#   facet_wrap(~ scenario) + 
#   guides(color = guide_legend(title = "demand-supply")) +
#   theme_bw() + 
#   theme(legend.position = "bottom",
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
#   
  
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
  date = c("After"),
  width_hint = 0.25,
  style = c("bar"),
  location = c("br"),
  unit_category = c("imperial"),
  text_col = c("black"),
  line_col = c("black")
)

ggplot() + 
  geom_sf(data = dc_scores_final, aes(fill = `desert status`), colour = NA) +
  geom_sf(data = wmata_shapes, color = "white") +
  geom_sf(data = wmata_states, fill = NA, col = "black") + 
  facet_wrap(~ scenario) + 
  # coord_sf(xlim = c(-77.3, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  coord_sf(xlim = c(1226715.965140128, 1369011.5944263502), ylim = c(376465.63918774325, 540355.250319933)) + 
  # scale_fill_manual(values = c("#440154FF", "#FDE725FF")) +
  scale_fill_manual(values = c("#66A182", "#2E4057")) +
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

# ggplot() + 
#   geom_sf(data = filter(dc_scores_final, !is.na(gap1)), aes(color = categ, fill = categ)) +
#   geom_sf(data = wmata_shapes, color = "white") + 
#   geom_sf(data = wmata_states, color = "black", fill = NA) + 
#   facet_wrap(~`desert status` + date) + 
#   # coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) +
#   coord_sf(xlim = c(1226715.965140128, 1369011.5944263502), ylim = c(376465.63918774325, 540355.250319933)) + 
#   scale_fill_viridis_d() + 
#   scale_color_viridis_d() + 
#   ggthemes::theme_map() + 
#   theme(panel.background = element_rect(fill = grey(0.9)))
# 
# ggsave("figures/gapTypes.png")

# Sufficiency analysis ---------------------------------------------------------

# Distribution of transit access
quantile(filter(dc_scores_final, date == "Feb. 2020")$score, c(seq(0.1, 0.5, 0.1)))

acc_quantiles <- 
  Hmisc::wtd.quantile(
    filter(dc_scores_final, scenario == "Before")$score,
    weights = dc_scores_final$pop_total, 
    probs=c( seq(0 , 1 , 0.1) ), 
    type=c('quantile'), 
    normwt = FALSE, 
    na.rm=T)

sufficiency <- 
  dc_scores_final %>%
  st_drop_geometry() %>%
  mutate(acc_quant = findInterval(score, acc_quantiles[-length(acc_quantiles)]))

# it's not correct to just total everyone in that category - it's cumulative
# has to be all in that quantile and below

suff_table <- 
  sufficiency %>%
    st_drop_geometry() %>%
    group_by(acc_quant, scenario) %>%
    summarize(total_pop = sum(pop_total),
              black_pop = sum(pop_black),
              white_pop = sum(pop_white),
              asian_pop = sum(pop_asiapacific),
              latinx_pop = sum(pop_hispanic),
              indig_pop = sum(pop_indig),
              pov_pop = sum(pop_poverty)) %>%
  arrange(scenario, acc_quant)

# We want a figure that shows changes in the FGT measures from feb-june by demographic
# group by thresholds 

suff_long <- 
  pivot_longer(sufficiency, 
               cols = hhld_nocar:workers_essential,
               names_to = "pop_group",
               values_to = "pop_total")

fgt <- function(df, threshold, grouping_var) {
  totals <- 
    df %>%
    group_by(eval(parse(text  = grouping_var))) %>%
    summarize(pop_total = sum(pop_total) / 2) # to account for two dates
  
  headcount <-
    df %>%
    group_by(scenario, eval(parse(text  = grouping_var))) %>%
    filter(score <= threshold) %>%
    summarize(fgt0_toSum = sum(pop_total),
              fgt1_toSum = sum(pop_total * ((threshold - score)/threshold)),
              fgt2_toSum = sum(pop_total * ((threshold - score)/threshold)^2))
  
  fg <- mutate(headcount, 
               fgt0 = fgt0_toSum / totals$pop_total,
               fgt1 = fgt1_toSum / totals$pop_total,
               fgt2 = fgt2_toSum / totals$pop_total,
               quant = threshold)
}

# foo <- fgt(suff_long, 65159.75, "pop_group")
bar_long <- 
  fgt(suff_long, 135366.5, "dec_inc") %>% # 50th percentile 
  pivot_longer(cols = c("fgt0", "fgt1", "fgt2")) %>%
  mutate(decile = `eval(parse(text = grouping_var))`) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(decile)) %>%
  mutate(exps = ifelse(name == "fgt0", "FGT\u2080",
                ifelse(name == "fgt1", "FGT\u2081", "FGT\u2082")))


bar_wide <- 
  fgt(suff_long, 135366.5, "dec_inc") %>% # 50th percentile 
  # pivot_longer(cols = c("fgt0", "fgt1", "fgt2")) %>%
  mutate(decile = `eval(parse(text = grouping_var))`)

segments <-
  pivot_wider(select(bar_long, scenario, name, value, decile),
              names_from = scenario,
              values_from = value) %>%
  mutate(exps = ifelse(name == "fgt0", "FGT\u2080",
                ifelse(name == "fgt1", "FGT\u2081", "FGT\u2082")))


ggplot() + 
  geom_segment(data = segments,
               aes(x = `Before`, xend = `After`, y = decile, yend = decile),
               color = grey(0.5)) +
  geom_point(data = bar_long, aes(y = decile, x = value, color = scenario), size = 2.5) +
  facet_wrap(~ exps) + 
  scale_color_manual(values = c("#66A182", "#2E4057")) +
  scale_y_discrete(limits=rev) +
  xlab(NULL) + ylab("income decile") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())
  
ggsave(filename="figures/fgt_compare.png")

# population totals in the categories ------------------------------------------
# 
# lollipop <- 
#   dc_scores_final %>%
#   # filter(score > 100000) %>%
#   st_drop_geometry() %>%
#   group_by(categ, date, `desert status`) %>%
#   summarize(totpop = sum(pop_total, na.rm = TRUE),
#             num_bgs = n(),
#             med_score = median(score)) %>%
#   arrange(categ, date, `desert status`)
# 
# ggplot(lollipop) +
#   geom_point(aes(x = totpop, y = categ, color = `desert status`))
# 
# 
# ggplot(lollipop) + 
#   geom_bar(aes(color = categ, fill = categ, x = totpop, y = `desert status`), stat = "identity") +
#   facet_wrap(~ date) + 
#   scale_fill_viridis_d() +
#   scale_color_viridis_d()
