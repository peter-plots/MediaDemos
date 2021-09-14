library(tidyverse)
library(ggh4x)

# install.packages(c("tidyverse", "ggh4x"))

df <- read_csv("Guardian Energy Plan/clean_electricity_sources.csv")

View(df)

str(df)

# replica
df %>%
  mutate(variable_name = factor(variable_name)) %>%
  mutate(power_source = recode_factor(df$variable_name,
                                      "Rooftop PV" = "Solar",
                                      "Utility-scale PV" = "Solar",
                                      "Onshore wind" = "Wind",
                                      "Offshore wind" = "Wind",
                                      "Nuclear" = "Nuclear",
                                      "Hydro" = "Hydro",
                                      "Biomass" = "Biomass",
                                      "Biomass w cc" = "Biomass",
                                      "Gas" = "Gas",
                                      "Gas w cc" = "Gas",
                                      "Coal" = "Coal",
                                      "Geothermal" = "Geothermal")) %>% 
  group_by(power_source, year) %>%
  summarize(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = power_source)) +
  geom_area(show.legend = FALSE,
            alpha = 0.85) +
  scale_fill_manual(values = c("#8BE2E1",
                               "#23BFB8",
                               "#FEE1B7",
                               "#D8DAEC",
                               "#B2ABD2",
                               "#AD9FB2",
                               "#787498",
                               "black")) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5),
                     limits = c(2017, 2050),
                     expand = expansion(mult = c(0, 0.03)),
                     guide = "axis_minor",
                     minor_breaks = seq(2020, 2050, by = 2.5)) +
  scale_y_continuous(breaks = seq(0, 9000, by = 1000),
                     expand = c(0, 0)) +
  annotate("text", 
           x = c(2023, 2027, 2042.5, rep(2049.9, 4)),
           y = c(400, 1150, 3500, 500, 750, 1250, 8300),
           label = c("Coal", "Gas", "Wind", "Biomass", "Hydro", "Nuclear", "Solar"),
           family = "Georgia",
           size = 3.5,
           hjust = 1) +
  annotate("text",
           x = 2017,
           y = seq(195, 9195, 1000),
           label = c(seq(0, 8000, by = 1000), "9000 TWh"),
           family = "Georgia",
           hjust = 0,
           color = "grey") +
  labs(x = "",
       y = "",
       title = "How sources of generated electricity would need to change in the\nnext 30 years",
       subtitle = "Projected change in energy sources based on a scenario where the US aggressively works to\nadopt electrification",
       caption = "Guardian graphic. Source: Princeton University's Net-Zero America report") +
  theme_minimal() +
  theme(ggh4x.axis.ticks.length.minor = rel(1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(family = "Helvetica",
                                    hjust = 0,
                                    color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.ticks.x = element_line(color = "grey"),
        axis.ticks.length.x = unit(1.5, "mm"),
        axis.text.x = element_text(color = "grey",
                                   size = 11),
        axis.line.x = element_line(color = "darkgrey",
                                   size = .5))


# remix

df %>%
  mutate(variable_name = factor(variable_name)) %>%
  mutate(power_source = recode_factor(df$variable_name,
                                      "Rooftop PV" = "Solar",
                                      "Utility-scale PV" = "Solar",
                                      "Onshore wind" = "Wind",
                                      "Offshore wind" = "Wind",
                                      "Nuclear" = "Nuclear",
                                      "Hydro" = "Hydro",
                                      "Biomass" = "Biomass",
                                      "Biomass w cc" = "Biomass",
                                      "Gas" = "Gas",
                                      "Gas w cc" = "Gas",
                                      "Coal" = "Coal",
                                      "Geothermal" = "Geothermal")) %>% 
  group_by(power_source, year) %>%
  summarize(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = power_source)) +
  geom_area(show.legend = FALSE,
            alpha = 0.85,
            color = "black") +
  scale_fill_manual(values = c("#8BE2E1",
                               "#23BFB8",
                               "#FEE1B7",
                               "#D8DAEC",
                               "#B2ABD2",
                               "#AD9FB2",
                               "#787498",
                               "black")) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5),
                     limits = c(2017, 2050),
                     expand = expansion(mult = c(0, 0.03)),
                     guide = "axis_minor",
                     minor_breaks = seq(2020, 2050, by = 2.5)) +
  scale_y_continuous(breaks = seq(0, 9000, by = 1000),
                     expand = c(0, 0)) +
  annotate("text", 
           x = c(2023, 2027, 2041, 2042.5, 2032.5, 2037, 2045),
           y = c(400, 1050, 3500, 400, 1630, 1900, 6600),
           label = c("Coal", "Gas", "Wind", "Biomass", "Hydro", "Nuclear", "Solar"),
           family = "Helvetica",
           size = 3.5,
           hjust = 1) +
  annotate("text",
           x = 2017,
           y = seq(195, 9195, 1000),
           label = c(seq(0, 8000, by = 1000), "9000 TWh"),
           family = "Helvetica",
           hjust = 0,
           color = "grey") +
  annotate("segment",
           x = 2041.5,
           y = 530,
           xend = 2044,
           yend = 720,
           size = .8,
           arrow = arrow(length = unit(2, "mm"))) +
  labs(x = "",
       y = "",
       title = "How sources of generated electricity would need to change in the\nnext 30 years",
       subtitle = "Projected change in energy sources based on a scenario where the US aggressively works to\nadopt electrification",
       caption = "PeterPlots graphic. Source: Princeton University's Net-Zero America report") +
  theme_minimal() +
  theme(ggh4x.axis.ticks.length.minor = rel(1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = "Helvetica"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(family = "Helvetica",
                                    hjust = 0,
                                    color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.ticks.x = element_line(color = "grey"),
        axis.ticks.length.x = unit(1.5, "mm"),
        axis.text.x = element_text(color = "grey",
                                   size = 11),
        axis.line.x = element_line(color = "darkgrey",
                                   size = .5))






