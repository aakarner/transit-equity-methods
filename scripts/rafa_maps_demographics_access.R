library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(ineq)
library(dineq)
library(patchwork)
options(scipen = 999)

# 1. read accessibility estimates --------------------------------------------------------------

# read ids of urban areas
urban_ids <- fread("output/urban.csv", colClasses = 'character')

# read accessibility estimates
dc_scores <- 
  st_read("output/dc_scores.geojson") |>
  st_transform("EPSG:4326") 

# keep only accessibility estimates in urban areas with pop > 0
setDT(dc_scores)
dc_scores <- dc_scores[ GEOID %in% urban_ids$bg_id,]
dc_scores <- dc_scores[ pop_total > 0,]


### sanity check

dc_scores[, .(w = weighted.mean(x= score, w=pop_white, na.rm=T),
              b = weighted.mean(x= score, w=pop_black, na.rm=T)), by = date ]



# 2. recode variables --------------------------------------------------------------

# find deciles
deciles  <- Hmisc::wtd.quantile(dc_scores$med_inc, weights=dc_scores$pop_total, 
                                probs=c( seq(0 , 1 , 0.1) ), 
                                type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                normwt=FALSE, na.rm=T)

dc_scores[, dec_inc := findInterval(med_inc , deciles[ -length(deciles) ] ) ]

dc_scores[, .(count = sum(pop_total)), by= dec_inc][order(count)]
dc_scores[, dec_inc := factor(dec_inc, 
                           levels = 10:1,
                           labels = c('D10\nWealthiest', 9:2, 'D1\nPoorest'))]

# create scenario variable
dc_scores[, scenario := fifelse(date == 'Feb. 2020', "Before", "After")]
dc_scores[, scenario := factor(scenario, 
                               levels = c("Before", "After"), 
                               labels = c("Before", "After"))]
table(dc_scores$scenario)


## calculate access impact
  
  # sort observations
  dc_scores <- dc_scores[order(GEOID , -date)]
  
  # calculate impact
  dc_scores[, difference := data.table::shift(score, type = "lag") - score,
            by = GEOID]
  
  head(dc_scores)
  
  # back to spatial sf
  dc_scores <- st_sf(dc_scores)




# 1. demographic maps --------------------------------------------------------------



# choropleth of black pop
fig_blacks <- ggplot() + 
  geom_sf(data = dc_scores, aes(fill = pop_black / pop_total), color = NA, ) + 
  # geom_sf(data = wmata_shapes, color = "black") + 
  # geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_distiller(name ='Share of Black\npopulation', palette = 'Purples', direction = 1, labels = scales::percent) + 
  ggthemes::theme_map()

# choropleth of white pop
fig_whites <- ggplot() + 
  geom_sf(data = dc_scores, aes(fill = pop_white / pop_total), color = NA, ) + 
  # geom_sf(data = wmata_shapes, color = "black") + 
  # geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_distiller(name ='Share of white\npopulation', palette = 'Oranges', direction = 1, labels = scales::percent) + 
  ggthemes::theme_map()


fig_demographics <- fig_blacks + fig_whites +  plot_annotation(tag_levels = 'A')

fig_demographics

ggsave(fig_demographics, 
       file = './figures/fig_demographics.png', 
       width = 16, height = 8, dpi = 200, units = 'cm')



# choropleth of income deciles
fig_deciles <- ggplot( data = subset(dc_scores, !is.na(dec_inc)) ) + 
  geom_sf(aes(fill = dec_inc), color = NA, ) + 
  # geom_sf(data = wmata_shapes, color = "black") + 
  # geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.5, -76.8), ylim = c(38.75, 39.2), expand = FALSE) + 
  scale_fill_brewer(name ='Incomde\ndeciles', palette = 'BrBG', direction = -1) + 
  ggthemes::theme_map()

fig_deciles
ggsave(fig_deciles, 
       file = './figures/fig_map_income.png', 
       width = 16, height = 16, dpi = 200, units = 'cm')






# 3. accessibility maps --------------------------------------------------------------

# choropleth of accessibility
fig_access_ba <- ggplot() +
  geom_sf(data = dc_scores, aes(fill = score), color = NA) + 
  geom_sf(data = wmata_states_clip, color = grey(0.5), fill = NA) +
  # geom_sf(data = wmata_shapes, color = "black") + 
  # geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  coord_sf(xlim = c(-77.6, -76.75), ylim = c(38.7, 39.2), expand = FALSE) + 
  scale_fill_viridis_c(
    name ='accessibility\n(jobs reachable in\n45 minutes on\npublic transit)', 
    labels = scales::comma, 
    direction = -1) + 
  facet_wrap(~ scenario, nrow = 1) +
  ggthemes::theme_map() +
  theme(strip.background = element_rect(fill=NA, color=NA))

max_value <- abs(dc_scores$difference) |> max(na.rm=T)
min_value <- -1 * max_value


fig_impact <- ggplot() +
  geom_sf(data = subset(dc_scores,!is.na(difference)), aes(fill = difference), color = NA) + 
  geom_sf(data = wmata_states_clip, color = grey(0.5), fill = NA) +
  # geom_sf(data = wmata_shapes, color = "black") + 
  # geom_sf(data = wmata_states, color = grey(0.5), fill = NA) + 
  scale_fill_distiller(name ='accessibility change\n(after - before)', palette = 'RdBu', 
                       direction = 1, labels = scales::comma,
                       limits = c(min_value, max_value)) + 
  coord_sf(xlim = c(-77.6, -76.75), ylim = c(38.7, 39.2), expand = FALSE) + 
  ggthemes::theme_map()

fig_access <- fig_access_ba / fig_impact + 
  plot_layout(ncol = 1, heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A')

fig_access

ggsave(fig_access, 
       file = 'figures/fig_access.png', 
       width = 16, height = 16, dpi = 200, units = 'cm')

