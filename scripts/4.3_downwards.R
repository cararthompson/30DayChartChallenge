# Time series - Downwards

## Load libraries ----
library(tidyverse)
library(xkcd)
library(extrafont)

## Read in and rework data ----
tibble(ratio = c(2/2, 2/3, 2/4, 2/4, 2/4),
       dates = lubridate::my(c("Apr-2011", "Jun-2019", "Apr-2021", 
                               # Need two points for Apr-2021 to draw lines to and 
                               # from it with different linetype
                               "Apr-2021", "Apr-2022")),
       era = factor(c(rep("past", 3), rep("future", 2)), 
                    levels = c("past", "future"))) %>%
  ggplot() +
  geom_step(aes(x = dates, y = ratio, linetype = era),
            show.legend = F) +
  scale_y_continuous(breaks = c(0.4, 0.5, 0.66, 1),
                     labels = c("", "0.5", "0.66", "1"),
                     limits = c(0.4, 1)) +
  theme_xkcd() %+replace%
  theme(axis.ticks.y = element_blank()) +
  labs(title = "\nRatio of adults to total number of people in our household\n",
       x = "",
       y = "",
       caption = "\n\n30DayChartChallenge   -   Graphic: cararthompson   -   Data: Our lives\n")

## Export plot ----
ggsave(filename = file.path("../plots", "4.3_downwards.png"), 
       dpi = 400, width = 8, height = 5)

