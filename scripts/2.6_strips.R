# Distributions - Strips

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)

## Read in data and plot it ---- 
read.csv("../data/mine/rainfall_orleans-wsm.csv") %>% 
  ggplot() +
  geom_jitter(aes(x = town, y = average_rainfall_days,
                  size = average_rainfall_mm),
             width = 0.1, height = 0,
             colour = "#27b2db", shape = "|", show.legend = F) +
  labs(y = "Average number of rainfall days per month",
       x = "",
       title = "\n«Pourquoi il pleut toujours dans ton pays?»",
       subtitle = "<br><br>Each droplet represents a different month of the year.
       <br>Its position on the Y axis represents the average number of 
       <br>days of rain in the month. Its size represents the average rainfall (in mm).
       <br>Turns out, when you compare Orleans and Weston-super-Mare,
       <br>it's really much of a muchness!<br>",
       caption = "#30DayChartChallenge | Graphic: @cararthompson | Source: weather-atlas.com") +
  ylim(c(10, NA)) +
  theme_minimal() %+replace%
  theme(panel.background = element_rect(colour = "#0f4657", fill = "#0f4657"),
        plot.background = element_rect(colour = "#0f4657", fill = "#0f4657"),
        text = element_text(colour = "#bfbfbf", family = "Segoe UI"),
        axis.text.y = element_text(colour = "#bfbfbf", size = 9),
        axis.text.x = element_text(colour = "#bfbfbf", size = 12),
        axis.title = element_text(colour = "#bfbfbf", size = 12),
        panel.grid = element_blank(),
        plot.title = element_text(size = 19, face = "bold", family = "Georgia"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        plot.caption = element_text(hjust = 0.5, colour = "#27b2db"))
  
## Export plot ----
ggsave(filename = file.path("../plots", "2.6_strips.png"), 
       dpi = 400, width = 8, height = 7)
