library(data.table)
library(tidycensus)
library(tigris)
library(forcats)
library(dplyr)
library(sf)
library(ggplot2)

# Map of urban block groups with relevant state/county boundaries

# read ids of urban areas
urban_ids <- 
  fread("output/urban.csv", colClasses = 'character') %>%
  mutate(trct_id = substr(bg_id, 1, 11))

urban_trcts <- unique(urban_ids$trct_id)

# WMATA service area: Arlington and Fairfax counties, the cities of Alexandria, 
# Fairfax, and Falls Church in Virginia, the District of Columbia, and 
# Montgomery and Prince George's counties in Maryland. 
# Source: https://www.arlingtonva.us/files/sharedassets/public/budget/documents/fy22-p-30-metro-02.20.21.pdf
# Extension to Dulles adds Loudoun county, Virginia, as well. 

# State FIPS codes:
# DC: 11
# MD: 24
# VA: 51
# WV: 54

state_ids <- c("11", "24", "51", "54")

wmata_area <- 
  counties(state = c("DC", "MD", "VA")) %>%
  filter(NAMELSAD %in% c("District of Columbia",
                         "Arlington County", "Fairfax County", "Loudoun County",
                         "Alexandria city", "Fairfax city", "Falls Church city", 
                         "Prince George's County") | 
         NAMELSAD == "Montgomery County" & STATEFP == 24) %>% # don't pull VA here
  st_union()
  # st_transform("EPSG:2248")

wmata_states <- 
  states() %>% 
  filter(
    NAME %in% c("Virginia", "Maryland", "District of Columbia"))

wmata_lines <- st_read("data/Metro_Lines_Regional.geojson")

# Pull demographics
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino
          
trct_race <- get_acs(geography = "tract", variables = hlstatusvars, 
                        state = state_ids,
                        year = 2018,
                        geometry = TRUE,
                        summary_var = "B03002_001") 

## Automobile ownership
trct_zvh <- get_acs(geography = "tract", variables = "B08201_002",
                       state = state_ids,
                       year = 2018,
                       geometry = TRUE,
                       summary_var = "B08201_001") # Total households
  
## Poverty
povvars <- c("C17002_002", # up to 50% of poverty
             "C17002_003", # 50%-99% poverty
             "C17002_004", # 1-1.24
             "C17002_005", # 1.25-1.49
             "C17002_006", # 1.5-1.84
             "C17002_007") # 1.85-1.99
trct_pov <- 
  get_acs(geography = "tract", 
          variables = povvars,
          state = state_ids,
          year = 2018,
          geometry = FALSE,
          summary_var = "C17002_001") %>%
  group_by(GEOID) %>%
  summarize(GEOID = first(GEOID),
            NAME = first(NAME),
            variable = "C17002",
            estimate = sum(estimate),
            moe = sqrt(sum(moe^2)),
            summary_est = first(summary_est),
            summary_moe = first(summary_moe))
  
trcts <- 
  rbind(
    tracts(state = "Virginia", year = 2018, cb = TRUE),
    tracts(state = "District of Columbia", year = 2018, cb = TRUE),
    tracts(state = "Maryland", year = 2018, cb = TRUE),
    tracts(state = "West Virginia", year = 2018, cb = TRUE))

trct_pov <- inner_join(select(trcts, GEOID, geometry), trct_pov)

## Single-occupancy vehicle commuting
trct_sov <- get_acs(geography = "tract", variables = "B08006_003",
                       state = state_ids,
                       year = 2018,
                       geometry = TRUE,
                       summary_var = "B08006_001") # Total households

# Flip the variable so that it represents non-drive-alone totals
trct_sov$estimate <- trct_sov$summary_est - trct_sov$estimate

## Combine all demographics

trct_demogs <- rbind(trct_race,
                     trct_zvh,
                     trct_pov,
                     trct_sov)

trct_demogs <- mutate(
  trct_demogs,
  pct = 100 * (estimate / summary_est),
  variable = fct_recode(variable,
    White = "B03002_003",
    Black = "B03002_004",
    Asian = "B03002_006",
    'Hispanic or Latino' = "B03002_012",
    'Zero-vehicle households' = "B08201_002",
    'Households in poverty' = "C17002",
    'Non-drive-alone commuters' = "B08006_003"))  %>%
  filter(GEOID %in% urban_trcts, variable != "Asian")

trct_demogs$brks <- 
  cut(trct_demogs$pct, 
      breaks=c(0, 20, 40, 60, 80, 100), right = FALSE, include.lowest = TRUE,
      labels=c("0 - 19", "20 - 39", "40 - 59", "60 - 79", "80 - 100"))

wmata_states_clip <- 
  st_intersection(wmata_states, st_union(trct_demogs))

ggplot(wmata_states_clip) + geom_sf(color = "red")

scale_params <- tibble::tibble(
    variable = c("Households in poverty"),
    width_hint = 0.25,
    style = c("bar"),
    location = c("br"),
    unit_category = c("imperial"),
    text_col = c("black"),
    line_col = c("black")
  )

na_params <- tibble::tibble(
  variable = c("Households in poverty")
)

plot_demogs <- st_transform(trct_demogs, "EPSG:2248")

ggplot() + 
  geom_sf(data = filter(plot_demogs, !is.na(brks)), aes(fill = brks), color = NA) + 
  geom_sf(data = wmata_states_clip, color = grey(0.5), fill = NA) +
  facet_wrap(~ variable, as.table = TRUE) +
  # xlab(NULL) + ylab(NULL) + 
  # geom_sf(data = wmata_lines, color = "white") + 
  # # geom_sf(data = cty_shapes_b5, aes(geometry = geometry), fill = NA, 
  #         color = grey(0.5), inherit.aes = FALSE) + 
  # coord_sf(xlim = c(-84.85071, -83.7991), ylim = c(34.18629, 33.35246)) +
  scale_fill_viridis_d(name = NULL, na.value = "grey75", direction = -1) +
  guides(fill = guide_legend("population share (%)", nrow = 1)) +
  theme_bw(base_size = 12) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        strip.background = element_rect(colour = "black", fill = "grey75"),
        strip.text.x = element_text(color = "black", face = "bold", size = 12),
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank(),
        legend.position = "bottom")

ggsave("output/DC_demographics.png")

# This scalebar and north arrow changes the order of the facets
# https://github.com/paleolimbot/ggspatial/issues/108
# +
#   ggspatial::annotation_scale(
#       aes(width_hint = width_hint, 
#           style = style, 
#           location = location, 
#           unit_category = unit_category,
#           text_col = text_col, 
#           line_col = line_col),
#       data = scale_params,
#       plot_unit = "ft") + 
#   ggspatial::annotation_north_arrow(data = na_params)

# Create inset map -------------------------------------------------------------

st_centroid(wmata_states)

cty_coords <- data.frame(
  X = c(-76.2, -78.6756, -78.2),
  Y = c(38.94624, 37.51784, 38.95),
  name_ = c("Maryland", "Virginia", "District of Columbia"))

ggplot() + 
  geom_sf(data = wmata_states) + 
  geom_sf(data = wmata_states_clip, fill = "#abcd66") +
  geom_label(
    data = cty_coords, 
    aes(x = X, y = Y, label = name_),
    size = 6,
    fontface = "italic") + 
  # coord_sf(xlim = c(-78,-76), ylim = c(38.5, 39.3)) + 
  ggthemes::theme_map(base_size = 15) 
  # theme(panel.grid.major = element_line(color = NA),
  #       panel.border = element_blank(),
  #       axis.text = element_blank(), axis.ticks = element_blank())

ggsave("output/DCinset.png", width = 3, height = 3.5)
