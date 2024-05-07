# Assemble master data frame of accessibilities and demographics

acc_demogs <- left_join(hex_demogs, st_drop_geometry(acc60))
acc_demogs$acc60[is.na(acc_demogs$acc60)] <- 0
acc_demogs_total <- filter(acc_demogs, variable == "B03002_003")

weighted.mean(acc_demogs_total$acc60, acc_demogs_total$totpop)
# 179,245
# This appears to be somewhat higher than the Access Across America findings
# because they include the *entire* nine-county MSA in their calculations

# Weighted right-skewed distribution of accessibilities 
ggplot(acc_demogs_total, aes(x = acc60, weight = totpop / (sum(totpop) / 2), color = when)) + 
  geom_density(binwidth = 25000, alpha = 0.25) + 
  theme_bw()

# Basic scatterplot of changes from before to after
ggplot(acc60_comp, aes(x = acc60_b, y = acc60_a)) + 
  geom_abline(slope = 1, intercept = 0, color = "red", size = 2) + 
  geom_point()

# WHere are the cells that gained accessibility? 
acc60_comp_hex <- inner_join(harris_hex, acc60_comp)

acc60_comp_hex <- mutate(acc60_comp_hex,
                         gains = ifelse(acc60_a > acc60_b, 1, 0))
  

ggplot(acc60_comp_hex, aes(col = gains, fill = gains)) + geom_sf()


# TODO: color by share of each racial/ethnic group

# Accessibility comparison maps 
to_plot <- acc_demogs %>%
  filter(variable == "B03002_003") %>%
  mutate(when = fct_rev(when)) 

comp_params <- tibble::tibble(
    when = c("after"),
    location = c("tl"),
    unit_category = c("imperial")
  )

ggplot() + 
  geom_sf(data = to_plot, 
          aes(fill = acc60, col = acc60)) + 
  # geom_sf(data = after_routes, color = "red") + 
  facet_wrap(~ when) + 
  xlab(NULL) + ylab(NULL) + 
  scale_fill_viridis_c(name = "cumulative opoortunities accessibility\n(jobs accessible within 60 minutes)") + 
  scale_color_viridis_c() + 
  guides(color = FALSE) + 
  theme_bw() + 
  annotation_scale(data = comp_params, text_cex = 1.0,
                   aes(location = location, unit_category = unit_category)) + 
  annotation_north_arrow(data = comp_params,
                         aes(location = "br"), which_north = "true", 
                         style = north_arrow_minimal) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank()) 

ggsave(here("output", "accessibility_comparison.png"))

# Examine accessibility changes
acc_comp_hex <- left_join(hex_demogs, acc60_comp)
acc_comp_hex$acc60_b[is.na(acc_comp_hex$acc60_b)] <- 0
acc_comp_hex$acc60_a[is.na(acc_comp_hex$acc60_a)] <- 0

acc_comp_hex$diff <- acc_comp_hex$acc60_a - acc_comp_hex$acc60_b

ggplot() + 
  geom_sf(data = acc_comp_hex, aes(col = diff, fill = diff)) +
  # geom_sf(data = after_routes, color = "red") + 
  scale_fill_viridis_c(name = "difference in cumulative\nopportunities accessibility\n(jobs accessible within 60 minutes)") + 
  scale_color_viridis_c() + 
  guides(color = FALSE) + 
  xlab(NULL) + ylab(NULL) + 
  theme_bw() + 
  annotation_scale(location = "tl", unit_category = "imperial", text_cex = 1.0) + 
   annotation_north_arrow(data = comp_params,
                         aes(location = "br"), which_north = "true", 
                         style = north_arrow_minimal) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank()) 

ggsave(here("output", "accessibility_changes.png"))

# Population-weighted changes-- entire service area
weighted_means <- 
  acc_demogs %>%
  st_drop_geometry() %>%
  mutate(race = fct_recode(variable,
                 white = "B03002_003",
                 Black = "B03002_004",
                 Latinx = "B03002_012",
                 Asian = "B03002_006")) %>%
  group_by(race, when) %>%
  summarize(wtd_mean = sum(est * acc60) / sum(est))

# Population-weighted changes-- only transit-accessible areas
weighted_means_tr <- 
  acc_demogs %>%
  st_drop_geometry() %>%
  mutate(race = fct_recode(variable,
                 white = "B03002_003",
                 Black = "B03002_004",
                 Latinx = "B03002_012",
                 Asian = "B03002_006")) %>%
  group_by(race, when) %>%
  filter(acc60 > 0) %>%
  summarize(wtd_mean = sum(est * acc60) / sum(est))

# Unweighted estimates
means <- 
  acc_demogs %>%
  st_drop_geometry() %>%
  group_by(when) %>%
  filter(acc60 != 0) %>%
  summarize(avg = mean(acc60))



# Lorenz curves-----------------------------------------------------------------

# These piped operations calculate the input data required to plot 
# Lorenz curves for each population group
lc_data <- acc_demogs %>%
  st_drop_geometry() %>%
  mutate(race = fct_recode(variable,
                 white = "B03002_003",
                 Black = "B03002_004",
                 Latinx = "B03002_012",
                 Asian = "B03002_006")) %>%
  group_by(variable, when) %>%
  arrange(acc60) %>%
  mutate(cum_acc = cumsum(acc60) / sum(acc60),
         cum_pop = cumsum(est) / sum(est))

# Gini coeffcients
ginis <- lc_data %>%
  group_by(race, when) %>%
  summarize(gc = gini(acc60, est))

ginis <- lc_data %>%
  group_by(when) %>%
  summarize(gc = gini(acc60, est))


# Accessibility "share" (no facet)
ggplot(filter(lc_data,  when == "before"),
  aes(x = cum_pop, y = cum_acc, col = race)) + 
  geom_line() + 
  xlab("population percentile") + 
  ylab("accessibility percentile") + 
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave(here("output", "gini_race_before.png"), height = 6, width = 10)

# Accessibility "share"
ggplot(lc_data,  
  aes(x = cum_pop, y = cum_acc, linetype = when)) + 
  geom_line(size = 1) + 
  facet_wrap(~ race) + 
  xlab("population percentile") + 
  ylab("accessibility percentile") + 
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave(here("output", "gini_race_relative_4x4.png"), height = 6, width = 10)

# Accessibility index
ggplot(lc_data,  
  aes(x = cum_pop, y = acc60 / 1000, linetype = when)) + 
  geom_line(size = 1) + 
  facet_wrap(~ race) + 
  xlab("population percentile") + 
  ylab("cumulative opportunities accessibility (60 min. threshold), 1000 total jobs") + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave(here("output", "gini_race_absolute_4x4.png"), height = 6, width = 10)


# Needs gaps -------------------------------------------------------------------

# trandep_vars <- c("B25046_001", # Aggregate vehicles available
#                  "B26001_001", # Group quarters population  
#                  "B26101_166", # Non-institutionalized group quarters pop
#                  "B23025_001", # Population aged >= 16 in workforce or not
#                  "B09001_008", # In households, 12-14 years old
#                  "B12001_001") # Total pop. > 15 years old 

# Demand variables
demand <- hex_trandep %>%
  spread(variable, est) %>%
  st_transform("+init=epsg:4326") %>%
  group_by() %>%
  mutate(demand_ct = ifelse(B23025_001 - B25046_001 > 0,
                            B23025_001 - B25046_001, 0),
         demand_shr = ifelse(B23025_001 != 0, demand_ct / B23025_001, 0),
         demand_pct = cut(demand_shr, breaks = seq(0, 1, 0.1), include.lowest = TRUE),
         demand_z = (demand_ct - mean(demand_ct, na.rm = TRUE)) / sd(demand_ct, na.rm = TRUE))

needs_gaps <- inner_join(filter(acc60, when == "before"), st_drop_geometry(demand)) %>%
  mutate(supply_z = (acc60 - mean(acc60, na.rm = TRUE)) / sd(acc60, na.rm = TRUE),
    gap = supply_z - demand_z,
    gap_d = cut(gap, breaks = c(-25, -5, -3, -1, 1, 3, 5)))


# How much transit supply is in "desert" areas? 
summary(filter(needs_gaps, as.integer(gap_d) == 1)$supply_z)
summary(filter(needs_gaps, as.integer(gap_d) == 1)$demand_z)

ds_comp <- gather(select(st_drop_geometry(needs_gaps), supply_z, demand_z))

ggplot() + 
  geom_density(data = ds_comp, aes(x = value, col = key)) +
  xlim(-5, 5)

# Plot "demand"
ggplot() + 
  geom_sf(data = demand, aes(col = demand_pct, fill = demand_pct)) + 
  geom_sf(data = bissonnet, col = "red", size = 1.25) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d() +
  annotation_scale(location = "tl", text_cex = 1.0, unit_category = c("imperial")) + 
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_minimal) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.background = element_blank()) 


# Plot "needs gaps"
ggplot() + 
  geom_sf(data = needs_gaps, aes(col = gap_d, fill = gap_d)) + 
  geom_sf(data = bissonnet, col = "red", size = 1.25) +
  scale_fill_viridis_d(name = "supply z-score minus demand z-score") + 
  scale_color_viridis_d() + 
  xlab(NULL) + ylab(NULL) +
  guides(color = FALSE) + 
  annotation_scale(location = "tl", text_cex = 1.0) + 
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_minimal) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.background = element_blank()) 

ggsave(here("output", "needs-gaps.png"))


# Identify route 65 Bissonnett
filter(metro_post$routes, route_short_name == "065")
bissonnet <- filter(metro_post$.$routes_sf, route_id == 28287)
demogs_latlong <- st_transform(hex_demogs, "+init=epsg:4326")

# Bisonnet close up demographics 
ggplot() + 
  geom_sf(data = demand, aes(col = demand_pct, fill = demand_pct)) + 
  #geom_sf(data = filter(demogs_latlong, variable == "B03002_003"), 
  #        aes(col = share, fill = share)) + 
  geom_sf(data = bissonnet, col = "red", size = 1.25) +
  xlab(NULL) + ylab(NULL) +
  coord_sf(xlim = c(-95.6144 * 1.0005, -95.38271 * 0.9995), 
           ylim = c(29.67412 * 0.9995, 29.73414 * 1.0005), expand = FALSE) + 
  scale_fill_viridis_d(name = "demand decile", labels = 1:10) + 
  scale_color_viridis_d() + 
  guides(color = FALSE) + 
  annotation_scale(location = "tl", text_cex = 1.0, unit_category = c("imperial")) + 
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_minimal) + 
  # scalebar(bissonnet, dist = 5, transform = FALSE, model = "WGS84", dist_unit = "mi") +
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.background = element_blank()) 

ggsave(here("output", "bissonnet_demand.png"), width = 8, height = 5)
