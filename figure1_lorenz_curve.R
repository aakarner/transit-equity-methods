library(ggplot2)
library(gglorenz)
library(tidyverse)

# prepare data
df <- billionaires %>%
        filter(Industry %in% c("Technology")) %>% 
        add_row(Industry = "Perfect Equality", TNW = 1)


# plot
fig_lorenz <- ggplot(data=df, aes(x = TNW, fill = Industry)) +
  stat_lorenz(geom = "area", alpha = 1, color='#0f3c53', size=1) +
  geom_abline(color='#0d8bb1', size=1) +
  scale_fill_manual(values = c('gray90', '#62ae9b'))+
  scale_x_continuous(expand = c(0,0), labels = scales::percent) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent, 
                     breaks = c(.25, .5, .75, 1)) + 
  annotate("text", x=.50, y=.3, parse=T, label = "bold(A)") + 
  annotate("text", x=.85, y=.3, parse=T, label = "bold(B)") + 
  annotate("text", x=.24, y=.6, parse=T, label = "bold(Gini==frac(A, B))", size=4) +
  annotate("text", x=.6, y=.88, parse=T, label = "'Line of equal distribution'") + 
  geom_segment(aes(x = .6, y = .85, xend = .72, yend = .75),
               arrow = arrow(length = unit(0.3, "cm"), type="closed")) +
  annotate("text", x=.65, y=.08, parse=T, label = "'Lorenz curve'") + 
  geom_segment(aes(x = .65, y = .09, xend = .64, yend = .20),
               arrow = arrow(length = unit(0.3, "cm"), type="closed")) +
  coord_fixed() +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none') +
  labs(x = "Cumulative share of people\nfrom lowest to highest income",
       y = "Cumulative share of income")

fig_lorenz


ggsave(fig_lorenz, 
       file = './figures/figure_1_lorenz.png', 
       width = 16, height = 15, units = 'cm', dpi = 300)
