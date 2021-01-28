# Axios story: https://www.axios.com/biden-electric-vehicles-infrastructure-924d4d85-280d-41ee-8449-fd62dd3ccecf.html
# DOE https://www.energy.gov/eere/vehicles/articles/fotw-1169-january-18-2021-vermont-had-highest-number-public-electric-vehicle
# Story date: January 21, 2021
# Story title: The starting place for Biden's electric vehicle charging push

library(readxl)
library(tidyverse)

df <- read_excel("fotw_1169_web.xlsx", sheet = 2)

df %>% view()

p2 <- df %>%
  mutate(quart = case_when(
    EV_char_per_reg > 1 ~ "5th",
    EV_char_per_reg > .75 & EV_char_per_reg <= 1 ~ "4th",
    EV_char_per_reg > .5 & EV_char_per_reg <= .75 ~ "3rd",
    EV_char_per_reg > .25 & EV_char_per_reg <= .5 ~ "2nd",
    TRUE ~ "1st"
  )) %>%
  ggplot(aes(x = reorder(State,
                         -EV_char_per_reg),
             y = EV_char_per_reg,
             fill = as.factor(quart))) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Greens",
                    labels = c("Less than .25",
                               "Between .25 & .5",
                               "Between .5 & .75",
                               "Between .75 & 1",
                               "More than 1 EV Charger\nper EV Registration")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Status of U.S. Electric Vehicle (EV) Infrastructure",
       subtitle = "EV Chargers per EV Registration in each State",
       fill = "Quantile Breakdown",
       caption = "Sources: U.S. Census & Department of Energy",
       x = "",
       y = "") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed",
                                      color = "black"),
    legend.position = c(.75, .65),
    legend.background = element_rect(fill = "white",
                                     color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90,
                               hjust = 1,
                               vjust = 0.4),
    text = element_text(family = "Georgia"),
    plot.title = element_text(face = "bold")
    )

ggsave("axios_ev_myversion.png",
       plot = p2,
       height = 4.69,
       width = 7.58,
       units = "in")

colorRampPalette("#199144")(3)

########################

p1 <- df %>%
  select(State, `EV Chargers per 100,000 People`, Population) %>%
  arrange(desc(Population, `EV Chargers per 100,000 People`)) %>%
  top_n(10) %>%
  ggplot(aes(x = State, 
             y = round(`EV Chargers per 100,000 People`, digits = 1))) +
  geom_col(fill = "#65e2a5", width = .75) +
  geom_text(aes(label = round(`EV Chargers per 100,000 People`, digits = 1)),
            hjust = -.2, color = "#199144") +
  coord_flip() +
  scale_x_discrete(limits = rev(c("CA", "GA", "NY", "FL", "NC",
                              "IL", "TX", "PA", "MI", "OH")),
                   labels = rev(c("California", "Georgia", "New York",
                                  "Florida", "North Carolina", "Illinois",
                                  "Texas", "Pennsylvania", "Michigan", "Ohio"))) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 80)) +
  labs(title = "Public EV chargers per 100k people",
       subtitle = "Deployment in the 10 most populous states",
       caption = "Data: Department of Energy; Chart: Axios Visuals") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(hjust = 0,
                               color = "black",
                               size = 10),
    plot.title = element_text(hjust = -.565,
                              face = "bold"),
    plot.subtitle = element_text(hjust = -.51),
    plot.caption = element_text(hjust = -.45)
  )

ggsave("axios_ev_replicate.png",
       plot = p1,
       width = 5.94,
       height = 3.64,
       units = "in")


extrafont::fonts()
