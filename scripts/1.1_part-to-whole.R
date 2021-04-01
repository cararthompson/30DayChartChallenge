# Comparisons - Part to whole

## Load libraries
library(tidyverse)
library(gggibbous)
library(extrafont)
library(ggtext)
library(cowplot)
library(ggfx)


## Read in and reorganise data
internet_access <- read.csv("../data/undata/UNdata_Export_20210314_internet-access-by-country-date.csv",
                            header = T, check.names = F)

ia_df <- internet_access %>%
  filter(Year == "2014") %>%
  mutate(Year = parse_number(Year),
         Value = parse_number(Value)) %>%
  arrange(desc(Value)) %>%
  mutate(Area = factor(`Country or Area`, levels = unique(`Country or Area`)))

ia_overall <- data.frame(`Area` = "Overall",
                         `Value` = median(ia_df$Value, na.rm = T))


## Create theme

light_col <- "#fff1af"
dark_col <- "#585350"
title_col <- "#fc8431"
text_col <- "#d0cfcd"
bg_col <- "#0d101f"

theme_dark_moon <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          text = element_text(colour = text_col, family = "Roboto", size = 8), 
          plot.title = element_text(hjust = 0.5, size = 25, colour = title_col, family = "Arvo"),
          axis.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = title_col, size = 10, family = "Arvo"),
          axis.title = element_blank(),
          strip.text = element_text(colour = text_col, size = 8),
          axis.ticks = element_blank())
}

## Plot it
# Overall internet access
overall <- ggplot(ia_overall,
                  aes(x = 0.9, y = 0.5)) +
  with_outer_glow(geom_moon(aes(ratio = Value / 100), size = 50, 
                            fill = light_col, colour = light_col),
                  expand = 5,
                  colour = light_col) +
  geom_moon(aes(ratio = 1 - (Value / 100)), size = 50, 
            fill = dark_col, colour = dark_col, alpha = 0.3, right = F) +
  geom_textbox(data = NULL,
               aes(x = 0.4, y = 0.5), width = 0.7,
               colour = text_col,
               box.colour = bg_col,
               size = 4.6,
               fill = bg_col,
               family = "Roboto",
               label = "The proportion of individuals using the internet varies hugely between different
               countries and geographical areas. 
               \nIn 2014 (the most recent year in the UN dataset), the median 
               <span style='color:#fff1af'>proportion of individuals using the internet</span> 
               in any given country or area was just shy of 45%. The recorded proportions vary from 98% to 0%. 
               For for all countries and areas in the top row,
               the proportion of individuals using the internet was over 90%,
               compared to under 5% for the bottom 16 countries and areas.") +
  xlim(c(0, 1)) +
  labs(title = "\nInternet use around the world\n") +
  theme_dark_moon()


# Access by country
by_area <- ggplot(ia_df,
                  aes(x = .5, y = .5)) +
  geom_moon(aes(ratio = Value / 100), 
            fill = light_col, colour = light_col, right = T) +
  geom_moon(aes(ratio = 1 - (Value / 100)), 
            fill = dark_col, colour = dark_col, alpha = 0.3, right = F) +
  facet_wrap(.~ Area, labeller = labeller(Area = label_wrap_gen(width = 12)), 
             ncol = 17) +
  labs(caption = "\n\n#30DayChartChallenge | Graphic: @cararthompson | Source: UNdata - International Telecommunications Union\n") +
  theme_dark_moon()



## Assemble plots 
p <- plot_grid(overall,
               by_area,
               ncol = 1,
               rel_heights = c(0.2, 0.8))


## Export plot ----
ggsave(plot = p, filename = file.path("../plots", "1.1_part-to-whole.png"), 
       dpi = 400, width = 13, height = 15)
