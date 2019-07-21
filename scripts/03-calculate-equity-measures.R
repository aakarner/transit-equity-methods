# Assemble master data frame of accessibilities and demographics

acc_demogs <- left_join(hex_demogs, st_drop_geometry(acc60))
acc_demogs$acc60[is.na(acc_demogs$acc60)] <- 0

acc_demogs_total <- filter(acc_demogs, variable == "B03002_003", acc60 != 0)

weighted.mean(acc_demogs_total$acc60, acc_demogs_total$totpop)
# This appears to be somewhat higher than the Access Across America findings
# because they include the *entire* nine-county MSA in their calculations

# Weighted right-skewed distribution of accessibilities 
ggplot(acc_demogs_total, aes(x = acc60, weight = totpop, color = when)) + 
  geom_histogram(binwidth = 25000, alpha = 0.25) + 
  theme_bw()

# Accessibility comparison maps 
ggplot(filter(acc_demogs, variable == "B03002_003"), aes(fill = acc60, col = acc60)) + 
  geom_sf() + 
  facet_wrap(~ fct_rev(when)) + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c() + 
  theme_bw()

ggsave(here("data", "accessibility_comparison.png"))

# Examine accessibility changes
acc_comp_hex <- left_join(hex_demogs, acc60_comp)
acc_comp_hex$acc60_b[is.na(acc_comp_hex$acc60_b)] <- 0
acc_comp_hex$acc60_a[is.na(acc_comp_hex$acc60_a)] <- 0

acc_comp_hex$diff <- acc_comp_hex$acc60_a - acc_comp_hex$acc60_b

ggplot(acc_comp_hex, aes(col = diff, fill = diff)) + 
  geom_sf() +
  scale_fill_viridis_c(name = "difference in cumulative\nopoortunities accessibility
                       \n(jobs accessible within 60 minutes)") + 
  scale_color_viridis_c() + 
  guides(color = FALSE) + 
  xlab(NULL) + ylab(NULL) + 
  theme_bw() + 
  scalebar(acc_comp_hex, dist = 10, transform = FALSE, model = "WGS84", dist_unit = "mi") +
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank()) 

ggsave(here("output", "accessibility_changes.png"))


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
  group_by(variable) %>%
  arrange(acc60) %>%
  mutate(cum_acc = cumsum(acc60) / sum(acc60),
         cum_pop = cumsum(est) / sum(est))


write.csv(foo, here("output", "black.csv"))

# Accessibility "share"
ggplot(lc_data,  
  aes(x = cum_pop, y = cum_acc, color = race)) + 
  geom_line() + 
  xlab("population percentile") + 
  ylab("accessibility percentile") + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave(here("output", "gini_race_relative.png"), height = 6, width = 10)

# Accessibility index
ggplot(lc_data,  
  aes(x = cum_pop, y = acc60 / 1000, color = race)) + 
  geom_line() + 
  xlab("population percentile") + 
  ylab("cumulative opportunities accessibility (60 min. threshold), 1000 total jobs") + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave(here("output", "gini_race_absolute.png"), height = 6, width = 10)


# Needs gaps -------------------------------------------------------------------



