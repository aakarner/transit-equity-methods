library(accessibility)
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
                                type=c('quantile'), 
                                normwt=FALSE, na.rm=T)

dc_scores[, deciles := findInterval(med_inc , deciles[ -length(deciles) ] ) ]

dc_scores[, .(count = sum(pop_total)), by= deciles][order(count)]
dc_scores[, dec_inc := factor(deciles, 
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
# dc_scores <- st_sf(dc_scores)




# 2. Inequality by race --------------------------------------------------------------


## 2.1 reshape data and load functions--------------------------------------------------------------

# load functions to calculate inequality
source('./06-0-inequality-measures.R')

df <- copy(dc_scores)
setDT(df)

# select colums and reshape to long format
df_race <- df[, .(GEOID, score, scenario, pop_black, pop_white, difference)]
df_race <- data.table::melt(data = df_race, 
                       id.vars = c('GEOID', 'score', 'scenario', 'difference'), 
                       variable.name = 'race',
                       value.name = 'pop')




# recode categories
df_race[, race := fifelse(race == "pop_black", "Blacks", "Whites")]



## 3.0 boxplot --------------------------------------------------------------

### level
box_level <- ggplot(data = df_race[pop > 0]) +
              geom_boxplot(
                aes(
                  x = race,
                  y = score,
                  color = race,
                  weight = pop,
                  group = race
                ),
                show.legend = FALSE
              ) +
              facet_wrap(~ scenario, nrow = 1) +
              labs(x = "Race", y = "Accessibility score") +
              theme_minimal()



### difference
box_impact <- ggplot(data = df_race[pop > 0]) +
  geom_boxplot(
    aes(
      x = race,
      y = difference,
      color = race,
      weight = pop,
      group = race
    ),
    show.legend = FALSE
  ) +
  labs(x = "Race", y = "Accessibility score") +
  theme_minimal()


fig_box_race <- box_level + box_impact + plot_annotation(tag_levels = 'A')
fig_box_race



ggsave(fig_box_race, 
       file = './figures/fig_box_race.png', 
       width = 20, height = 8, dpi = 200, units = 'cm')



## 2.2 lorenz curves --------------------------------------------------------------

# function to get a data.frame to plot the Lorenz curve
get_lorenz_df <- function(x, n){
  distrib <- ineq::Lc(x = x, n = n)
  temp_df <- data.frame(p = distrib$p, L = distrib$L)
  return(temp_df)
}

lorenz_race <- df_race[, get_lorenz_df(x = score, n = pop), by = .(race, scenario)]
lorenz_total <- df_race[, get_lorenz_df(x = score, n = pop),by = .(scenario)]
lorenz_total$race <- 'All'

# rbind
setcolorder(lorenz_total, names(lorenz_race))
lorenz_df <- rbind(lorenz_total, lorenz_race)

fig_lorenz_race <- ggplot(data=lorenz_df) +
                      geom_line(aes(x=p, y=L, color=race)) +
                      scale_x_continuous(name="Cumulative share of Population",
                                         expand = c(0, 0), labels = c(0, .25, .5, .75, 1)) + 
                      scale_y_continuous(name="Cumulative share of Access",
                                         expand = c(0, 0)) +
                      labs(color = 'Group') +
                      facet_wrap(~ scenario, nrow = 1) +
                      geom_abline() +
                      theme_classic() +
                      theme(strip.background = element_rect(fill=NA, color=NA))

fig_lorenz_race
# ggsave(fig_lorenz_race, 
#        file = './figures/fig_lorenz_race.png', 
#        width = 16, height = 8, dpi = 200, units = 'cm')





## 2.3 gini --------------------------------------------------------------

gini_all <- df_race[, .(gini = gini(x=score, w=pop),
                   race = 'All'), by = scenario]

gini_races <- df_race[, .(gini = gini(x=score, w=pop)), 
                 by = .(race, scenario)]

setcolorder(gini_all, names(gini_races))

gini_df <- rbind(gini_all, gini_races)

fig_gini <- ggplot() + 
            geom_col(data=gini_df, 
                     aes(x=race, y = gini , fill=scenario), position = "dodge") +
            labs(x='Group', y = 'Gini coef.', fill = "Scenario") +
            theme_minimal() 
            


fig_lorenz_gini_race <- (fig_lorenz_race / fig_gini )+ plot_annotation(tag_levels = 'A')
fig_lorenz_gini_race



ggsave(fig_lorenz_gini_race, 
       file = './figures/fig_lorenz_gini_race.png', 
       width = 18, height = 16, dpi = 200, units = 'cm')




## 2.4 palma ratio --------------------------------------------------------------


df_palma_race <- df_race[, .(avg_access = weighted.mean(x = score, w = pop, na.rm = TRUE)),
                    by = .(scenario   ,race)]

df_palma_race <- data.table::dcast(df_palma_race, 
                              formula = scenario ~ race, 
                              value.var = "avg_access")


df_palma_race$palma_race <- df_palma_race$Whites / df_palma_race$Blacks

fig_palma_race <- ggplot(data = df_palma_race, aes(x=scenario, y = palma_race)) + 
                    geom_col(aes(fill=scenario)) +
                    geom_text( aes(label = round(palma_race, digits = 2)),
                      vjust = 1.5,
                      color = "white",
                      size = 10
                    ) +
                    labs(x='Scenario', y = 'Racial ratio\nWhites / Blacks', fill = "Scenario") +
                    theme_minimal() +
                    theme(legend.position="none")

fig_palma_race
ggsave(fig_palma_race, 
       file = './figures/palma_race.png', 
       width = 15, height = 10, dpi = 200, units = 'cm')


## 2.5 theil --------------------------------------------------------------

# total inequality
theil_race_total <- df_race[, theil_t(x=score, w=pop), by = scenario]

# inequality components
temp_before <- subset(df_race, scenario == 'Before')
temp_after <- subset(df_race, scenario == 'After')

theil_race_comp_before <- decomp_theil_t(x = temp_before$score, 
                                        groups = temp_before$race, 
                                        w = temp_before$pop)

theil_race_comp_after <- decomp_theil_t(x = temp_after$score, 
                                        groups = temp_after$race, 
                                        w = temp_after$pop)

# add scenarios
theil_race_comp_before[[1]]$scenario <- 'Before'
theil_race_comp_before[[2]]$scenario <- 'Before'
theil_race_comp_after[[1]]$scenario <- 'After'
theil_race_comp_after[[2]]$scenario <- 'After'

theil_all_btw <- rbind(theil_race_comp_before[[1]], theil_race_comp_after[[1]])
theil_within <- rbind(theil_race_comp_before[[2]], theil_race_comp_after[[2]])

theil_all_btw[, scenario := factor(scenario, 
                                   levels = c('Before', 'After'),
                                   labels = c('Before', 'After'))]
theil_within[, scenario := factor(scenario, 
                                   levels = c('Before', 'After'),
                                   labels = c('Before', 'After'))]

fig_theil_total <- ggplot() + 
                   geom_col(data = subset(theil_all_btw, component == 'total'),
                            aes(x=scenario, y = value , fill=scenario)) +
                  labs(x=' ', y = 'Total inequality', fill = "group") +
                  theme_minimal() + 
                  theme(legend.position="none") 


fig_theil_btwn <- ggplot() + 
                  geom_col(data = subset(theil_all_btw, component == 'between'),
                           aes(x=scenario, y = value , fill=scenario)) +
                  labs(x=' ', y = 'Between-group inequality', fill = "group") +
                  theme_minimal() + 
                  theme(legend.position="none") 

fig_theil_within <- ggplot() + 
                      geom_col(data=theil_within, 
                               aes(x=group, y = within_i , fill=scenario), position = "dodge") +
                      labs(x='Group', y = 'Within-group inequality', fill = "Scenario") +
                      # facet_wrap(~ group, nrow = 1) +
                      theme_minimal() +
                      # theme_classic
                      theme(legend.position="bottom") 
                    


fig_theil_race <- (fig_theil_total + fig_theil_btwn) / fig_theil_within +
                  plot_annotation(tag_levels = 'A')

fig_theil_race

ggsave(fig_theil_race, 
       file = './figures/fig_theil_race.png', 
       width =16, height = 16, dpi = 200, units = 'cm')








# 3. Inequality by income --------------------------------------------------------------


## 3.0 reshape data and load functions--------------------------------------------------------------

# load functions to calculate inequality
source('./06-0-inequality-measures.R')

df <- copy(dc_scores)
setDT(df)


# select colums and reshape to long format
df_inc <- df[, .(GEOID, score, med_inc, deciles, dec_inc, scenario, date, pop_total, difference)]
df_inc <- df_inc[!is.na(dec_inc)]
table(df_inc$dec_inc, useNA = "always")




## 3.1 boxplot --------------------------------------------------------------

### level
box_level <- ggplot(data = df_inc[pop_total > 0]) +
  geom_boxplot(
    aes(
      x = dec_inc,
      y = score,
      color = dec_inc,
      weight = pop_total,
      group = dec_inc
    ),
    show.legend = FALSE
  ) +
  facet_wrap(~ scenario, nrow = 1) +
  scale_x_discrete(limits=rev) +
  scale_colour_brewer(palette = "BrBG") +
  labs(x = "Income decile", y = "Accessibility score") +
  theme_minimal()



### difference
box_impact <- ggplot(data = df_inc[pop_total > 0]) +
  geom_boxplot(
    aes(
      x = dec_inc,
      y = difference,
      color = dec_inc,
      weight = pop_total,
      group = dec_inc
    ),
    show.legend = FALSE
  ) +
  scale_x_discrete(limits=rev) +
  scale_colour_brewer(palette = "BrBG") +
  labs(x = "Income decile", y = "Accessibility score") +
  theme_minimal()


fig_box_income <- box_level / box_impact + plot_annotation(tag_levels = 'A')
fig_box_income



ggsave(fig_box_income, 
       file = './figures/fig_box_income.png', 
       width = 18, height = 16, dpi = 200, units = 'cm')



## 3.2 lorenz curves --------------------------------------------------------------

# function to get a data.frame to plot the Lorenz curve
get_lorenz_df <- function(x, n){
  distrib <- ineq::Lc(x = x, n = n)
  temp_df <- data.frame(p = distrib$p, L = distrib$L)
  return(temp_df)
}

# lorenz_inc <- df_inc[, get_lorenz_df(x = score, n = pop_total), by = .(dec_inc, scenario)]
lorenz_total <- df_inc[, get_lorenz_df(x = score, n = pop_total),by = .(scenario)]
lorenz_total$dec_inc <- 'All'

# rbind
setcolorder(lorenz_total, names(lorenz_inc))
lorenz_df <- rbind(lorenz_total) # , lorenz_inc

fig_lorenz_inc <- 
  ggplot(data= lorenz_df ) + # lorenz_inc
  geom_line(aes(x=p, y=L, color=scenario)) + # color=dec_inc
  scale_x_continuous(name="Cumulative share of population\nranked by accessibility level",
                     expand = c(0, 0), labels = c(0, .25, .5, .75, 1)) + 
  scale_y_continuous(name="Cumulative share of Access",
                     expand = c(0, 0)) +
  labs(color = '') + # Group
#  scale_color_brewer(name ='Incomde\ndeciles', palette = 'BrBG', direction = -1) + 
#  facet_wrap(~ scenario, nrow = 1) +
  geom_abline(linetype = "dashed") +
  coord_fixed()  +
  theme_minimal() +
  theme(strip.background = element_rect(fill=NA, color=NA)) 





fig_lorenz_inc
# ggsave(fig_lorenz_inc,
#        file = './figures/fig_lorenz_inc.png',
#        width = 16, height = 8, dpi = 200, units = 'cm')
# 
# 



## 3.3 gini --------------------------------------------------------------

acs <- copy(df_inc)[, .(GEOID, score, scenario, deciles)]
acs <- unique(acs)
setnames(acs, 'GEOID', 'id')

pop <- copy(df_inc)[, .(GEOID, med_inc, deciles, pop_total )]
pop <- unique(pop)
setnames(pop, 'GEOID', 'id')

gini_all <- accessibility::gini_index(accessibility_data = acs,
                          sociodemographic_data = pop, 
                          opportunity = 'score',
                          population = 'pop_total', 
                          group_by = 'scenario')


gini_inc <- accessibility::gini_index(accessibility_data = acs,
                                      sociodemographic_data = pop, 
                                      opportunity = 'score',
                                      population = 'pop_total', 
                                      group_by = c('deciles','scenario')
                                      )

gini_inc[, deciles := factor(deciles,
                             levels = 10:1,
                             labels = c('D10\nWealthiest', 9:2, 'D1\nPoorest'))]

# gini_all$deciles <- 'All'
# setcolorder(gini_all, names(gini_inc))
# 
# gini_all$fact <- 'all'
# gini_inc$fact <- 'groups'
# 
# gini_df <- rbind(gini_all, gini_inc)

fig_gini_all <- ggplot() + 
                  geom_col(data = gini_all, 
                           aes(x = scenario, 
                               y = gini_index, 
                               fill=scenario), 
                           position = "dodge") +
                  labs(x='', y = 'Gini coef.', fill = "") +
                  ylim(0, .65) + 
                  # scale_x_discrete(limits=rev) +
                  theme_minimal() +
                  theme(axis.title.x=element_blank()) +
                  theme(legend.position="none") 


fig_gini_dec <- ggplot() + 
  geom_col(data = gini_inc, 
           aes(x = deciles, 
               y = gini_index, 
               fill=scenario), 
           position = "dodge") +
  labs(x='', y = 'Gini coef.', fill = "") +
  ylim(0, .65) + 
  # scale_x_discrete(limits=rev) +
  theme_minimal() +
  theme(axis.title.x=element_blank())



fig_lorenz_gini_inc <- ((fig_lorenz_inc + fig_gini_all) / fig_gini_dec ) + 
                        plot_annotation(tag_levels = 'A')
fig_lorenz_gini_inc



ggsave(fig_lorenz_gini_inc, 
       file = './figures/fig_lorenz_gini_inc2.png', 
       width = 18, height = 16, dpi = 200, units = 'cm')




## 3.4 palma ratio --------------------------------------------------------------
acs <- copy(df_inc)[, .(GEOID, score, scenario )]
setnames(acs, 'GEOID', 'id')

pop <- copy(df_inc)[, .(GEOID, med_inc, deciles, pop_total )]
setnames(pop, 'GEOID', 'id')

df_palma_inc <- accessibility::palma_ratio(accessibility_data = acs,
                                      sociodemographic_data = pop, 
                                      opportunity = 'score',
                                      population = 'pop_total',
                                      income = 'med_inc',
                                      group_by = 'scenario')

# plot
fig_palma_inc <- 
  ggplot(data = df_palma_inc, aes(x=scenario, y = palma_ratio)) + 
  geom_col(aes(fill=scenario)) +
  geom_text( aes(label = round(palma_ratio, digits = 2)),
             vjust = 1.5,
             color = "white",
             size = 5
  ) +
  labs(x='Scenario', y = 'Pala ratio', fill = "") +
  theme_minimal() +
  theme(legend.position="none")


fig_palma_inc

ggsave(fig_palma_inc, 
       file = './figures/palma_inc.png', 
       width = 15, height = 10, dpi = 200, units = 'cm')



## 3.5 theil --------------------------------------------------------------
acs <- copy(df_inc)[, .(GEOID, score, scenario, deciles)]
acs <- unique(acs)
setnames(acs, 'GEOID', 'id')

pop <- copy(df_inc)[, .(GEOID, med_inc, scenario, deciles, pop_total )]
pop <- unique(pop)
setnames(pop, 'GEOID', 'id')


accessibility::theil_index(accessibility_data = acs,
                           sociodemographic_data = pop,
                           opportunity = 'score',
                           population = 'pop_total',
                           socioeconomic_groups = 'deciles')#,
                           group_by = 'scenario')

# total inequality
theil_dec_inc_total <- df_inc[, theil_t(x=score, w=pop_total), by = scenario]

# inequality components
temp_before <- subset(df_inc, scenario == 'Before')
temp_after <- subset(df_inc, scenario == 'After')

theil_dec_inc_comp_before <- decomp_theil_t(x = temp_before$score, 
                                         groups = temp_before$dec_inc, 
                                         w = temp_before$pop_total)


theil_dec_inc_comp_after <- decomp_theil_t(x = temp_after$score, 
                                        groups = temp_after$dec_inc, 
                                        w = temp_after$pop_total)

# add scenarios
theil_dec_inc_comp_before[[1]]$scenario <- 'Before'
theil_dec_inc_comp_before[[2]]$scenario <- 'Before'
theil_dec_inc_comp_after[[1]]$scenario <- 'After'
theil_dec_inc_comp_after[[2]]$scenario <- 'After'

theil_all_btw <- rbind(theil_dec_inc_comp_before[[1]], theil_dec_inc_comp_after[[1]])
theil_within <- rbind(theil_dec_inc_comp_before[[2]], theil_dec_inc_comp_after[[2]])

theil_all_btw[, component := factor(component, 
                                   levels = c('total', 'within', 'between'),
                                   labels = c('Total', 'Within', 'Between'))]

theil_all_btw[, scenario := factor(scenario, 
                              levels = c('Before', 'After'),
                              labels = c('Before', 'After'))]
theil_within[, scenario := factor(scenario, 
                                   levels = c('Before', 'After'),
                                   labels = c('Before', 'After'))]

theil_within[, group := factor(group, 
                              levels = c('D1\nPoorest', 2:9, 'D10\nWealthiest'),
                              labels = c('D1\nPoorest', 2:9, 'D10\nWealthiest'))]


fig_theil_total2 <- ggplot() + 
                    geom_col(data = theil_all_btw,
                             position = "dodge",
                             aes(x=component, y = value , fill=scenario)) +
                    labs(x='Inequality component', y = 'Theil inequality', fill = "group") +
                    theme_minimal() + 
                    theme(legend.position="top",
                          legend.title=element_blank()) 

fig_theil_total <- ggplot() + 
                    geom_col(data = subset(theil_all_btw, component == 'Total'),
                             aes(x=scenario, y = value , fill=scenario)) +
                    labs(x=' ', y = 'Total inequality', fill = "group") +
                    theme_minimal() + 
                    theme(legend.position="none") 


fig_theil_btwn <- ggplot() + 
                  geom_col(data = subset(theil_all_btw, component == 'Between'),
                           aes(x=scenario, y = value , fill=scenario)) +
                  labs(x=' ', y = 'Between-group inequality', fill = "group") +
                  theme_minimal() + 
                  theme(legend.position="none") 
                
fig_theil_within <- ggplot() + 
                  geom_col(data=theil_within, 
                           aes(x=group, y = within_i , fill=scenario), position = "dodge") +
                  labs(x='Group', y = 'Within-group inequality', fill = "Scenario") +
                  # facet_wrap(~ group, nrow = 1) +
                  scale_x_discrete(limits=rev) +
                  theme_minimal() +
                  # theme_classic
                  theme(legend.position="bottom") 



fig_theil_dec_inc <- (fig_theil_total + fig_theil_btwn) / fig_theil_within +
  plot_annotation(tag_levels = 'A')

fig_theil_dec_inc

ggsave(fig_theil_dec_inc, 
       file = './figures/fig_theil_inc.png', 
       width =16, height = 16, dpi = 200, units = 'cm')

ggsave(fig_theil_total2, 
       file = './figures/fig_theil_inc2.png', 
       width =16, height = 10, dpi = 200, units = 'cm')



#' Because we use a place-based accessibility, we are implicitly assuming that 
#'assume all individuals in the same census block have the same accessibility
#'level, regardeless of personal characteristics. This is why place-based 
#'accessibility measures are known to underestimate accessibility inequalities
#'Kwan , Neutens



## 3.6 concentration index --------------------------------------------------------------

acs <- copy(df_inc)[, .(GEOID, score, scenario)]
acs <- unique(acs)
setnames(acs, 'GEOID', 'id')

pop <- copy(df_inc)[, .(GEOID, med_inc, deciles, pop_total )]
pop <- unique(pop)
setnames(pop, 'GEOID', 'id')

ci_all <- accessibility::concentration_index(accessibility_data = acs,
                                      sociodemographic_data = pop, 
                                      opportunity = 'score',
                                      population = 'pop_total', 
                                      income = 'med_inc',
                                      type = 'corrected',
                                      group_by = 'scenario')

ci_all_dec <- accessibility::concentration_index(accessibility_data = acs,
                                                 sociodemographic_data = pop, 
                                                 opportunity = 'score',
                                                 population = 'pop_total', 
                                                 income = 'deciles',
                                                 type = 'corrected',
                                                 group_by = 'scenario')

# plot
fig_ci <- 
  ggplot(data = ci_all, aes(x=scenario, y = concentration_index)) + 
  geom_col(aes(fill=scenario)) +
  geom_text( aes(label = round(concentration_index, digits = 2)),
             vjust = 1.5,
             color = "white",
             size = 5
  ) +
  labs(x='Scenario', y = 'Concentration index', fill = "") +
  theme_minimal() +
  theme(legend.position="none")


fig_ci

ggsave(fig_ci, 
       file = './figures/fig_conc_index.png', 
       width = 15, height = 10, dpi = 200, units = 'cm')


fig_ci_inc <- fig_conc_curve + fig_ci + plot_annotation(tag_levels = 'A')
fig_ci_inc

ggsave(fig_ci_inc, 
       file = './figures/fig_conc_index.png', 
       width = 20, height = 8, dpi = 200, units = 'cm')


# df_inc[scenario=='Before', # 'After'
#             rineq::ci( ineqvar = med_inc,
#                        outcome = score,
#                        weights=  pop_total, 
#                        method='direct',
#                        type= 'CI') # 'CI' 'CIg'
#             ]
# conc_index <- df_inc[, concentr(x= score, y=med_inc, w= pop_total), by =scenario]

# # decompose concentration index
# temp_before <- subset(df_inc, scenario == 'Before')
# temp_after <- subset(df_inc, scenario == 'After')
# 
# decomp_concentr(x = temp_before$score, 
#                 y = temp_before$med_inc,
#                 nonOverlaping_groups = temp_before$deciles, 
#                 w= temp_before$pop_total)
# 
# decomp_concentr(x = temp_after$score, 
#                 y = temp_after$med_inc,
#                 nonOverlaping_groups = temp_after$deciles, 
#                 w= temp_after$pop_total)


##### 3.6.1 concentration curve ------------------------
temp_after <- temp_after[order(med_inc, deciles, score)]
temp_before <- temp_before[order(med_inc, deciles, score)]

get_conc_curve_df <- function(dt, income, population, access, group_by){
  
  dt <- dt[order(get(group_by), get(income), get(population), get(access))]

  temp_df <- dt[, .(x = cumsum(get(population))/max(cumsum(get(population))),
                    y = cumsum(get(access)*get(population))/max(cumsum(get(access)*get(population)))
                    ),
                by = group_by]
  return(temp_df)
  }


df_conc_curve <- get_conc_curve_df(df_inc, 
                  income = 'med_inc',
                  population = 'pop_total',
                  access = 'score',
                  group_by = 'scenario')


fig_conc_curve <- ggplot(data=df_conc_curve) +
                    geom_line(aes(x=x, y=y, color = scenario)) +
                    scale_x_continuous(name="Cumulative share of population\nsorted by income",
                                       expand = c(0, 0), labels = scales::percent) + 
                    scale_y_continuous(name="Cumulative share of Access",
                                       expand = c(0, 0), labels = scales::percent) +
                    labs(color = '') +
                    # scale_color_brewer(name ='Incomde\ndeciles', palette = 'BrBG', direction = -1) + 
                    # facet_wrap(~ scenario, nrow = 1) +
                    geom_abline(linetype = "dashed") +
                    theme_minimal() +
                    coord_fixed()  +
                    theme(axis.line = element_line(colour = "gray90", linetype=1))

fig_conc_curve

ggsave(fig_conc_curve, 
       file = './figures/fig_conc_curve.png', 
       width =16, height = 16, dpi = 200, units = 'cm')


# sanity check
curveConcent(temp_after$score, temp_after$med_inc, w= temp_after$pop_total)
curveConcent(temp_before$score, temp_before$deciles, w= temp_before$pop_total, add=T, col='red')
