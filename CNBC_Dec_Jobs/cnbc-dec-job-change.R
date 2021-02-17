# Title: Here’s where the jobs are — in one chart
# Author: Thomas Franck
# url: https://www.cnbc.com/2021/01/08/where-the-jobs-are-december-2020-chart.html

library(tidyverse)


df <- data.frame(
  sector = c("Professional and business svcs",
             "Retail trade",
             "Construction",
             "Transportation and warehousing",
             "Manufacturing",
             "Health care and social assistance",
             "Wholesale trade",
             "Financial activities",
             "Mining and logging",
             "Utilities",
             "Information",
             "Government",
             "Leisure and hospitality"),
  net_change = c(161000, 120500, 51000, 46600, 38000,
             32000, 25100, 12000, 4000, -400, -1000,
             -45000, -498000)
)

p <- df %>%
  mutate(pos = net_change > 0) %>%
  ggplot(aes(x = reorder(sector, net_change),
             y = net_change,
             fill = pos)) +
  geom_col() +
  scale_y_continuous(limits = c(-500000, 220000),
                     breaks = c(-500000, -400000, -300000,
                                -200000, -100000,
                                0, 100000, 200000),
                     labels = c("-500K", "-400K", "-300K", "-200K",
                                "-100K", "0", "100K", "200K"),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("#81b2d5", "#002f6c")) +
  labs(x = "",
       y = "",
       title = "December jobs one-month net change",
       caption = "SOURCE: Bureau of Labor Statistics") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "darkgrey"),
    plot.title = element_text(face = "bold",
                              hjust = 32),
    axis.line.x = element_blank(),
    axis.line.y = element_line(color = "darkgrey"),
    panel.grid.major.x = element_line(),
    plot.caption = element_text(hjust = -1.5),
    legend.position = "none"
  )

ggsave("cnbc-dec-jobs.png",
       plot = p,
       width = 5.49,
       height = 3.94,
       units = "in")





