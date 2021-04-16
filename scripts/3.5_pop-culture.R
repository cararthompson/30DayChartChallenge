# Relationships - Pop culture

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggbump)

## Read in and rework data ----
fm_df <- read.csv("../data/mine/fleetwood-mac.csv")

fm_df <-  fm_df %>%
  mutate(band_colour = case_when(band_member %in% c("Mick Fleetwood",
                                          "John McVie",
                                          "Lindsey Buckingham") ~ "Male",
                                 band_member %in% c("Christine McVie", 
                                 "Stevie Nicks") ~ "Female",
                                 TRUE ~ "Temp")) %>%
  group_by(band_member) %>%
  mutate(entry = case_when(year == min(year) ~ "Entry"))
  
  
ggplot(fm_df, aes(year, rank, colour = band_colour, 
                  group = band_member)) +
  geom_rect(data = NULL, aes(xmin = 1965, xmax = 2020,
                             ymin = -2, ymax = 6),
            fill = NA) +
  geom_bump(smooth = 10, size = 1.5, alpha = 0.2) +
  geom_bump(data = fm_df %>% filter(instrument != "Exit"),
            smooth = 10, alpha = 0.8, size = 1.5) + 
  scale_y_continuous(breaks = sort(c(unique(fm_df$rank), 7)),
                   labels  = c("Gone their own way", "Taking a break", 
                               "Guitar 3", "Guitar 2", "Drums", 
                               "Bass", "", "", "", "", 
                               "Keys", "", "", "Guitar 1", "", "", "Vocals", 
                               "", "Going solo", "")) +
  geom_point(data = fm_df %>% filter(entry == "Entry"),
             aes(x = year - .2, fill = band_colour),
             size = 4,
             shape = 21) +
  geom_textbox(data = fm_df %>% filter(entry == "Entry"), 
            aes(label = band_member, 
                x = year-.2),
            colour = "#1a1816",
            box.colour = "#fbf3dc",
            fill = "#fbf3dc",
            box.padding = unit(c(0,0,0,0), "pt"),
            width = NULL, box.r = unit(0.1, "pt"),
            alpha = 0.7,
            nudge_y = .4,
            nudge_x = -.4,
            size = 2.5,
            family = "Didot",
            fontface = "bold",
            hjust = 0) +
  scale_colour_manual(values = c("#6e665e", "#1a1816", "#b9b3ac")) +
  scale_fill_manual(values = c("#6e665e", "#1a1816", "#b9b3ac")) +
  labs(x = "", y = "",
       title = "\nLoving you isn't the right thing to do",
       subtitle = "\nBand members, instruments and relationships within Fleetwood Mac from its foundation to 2018\n\n",
       caption = "#30DayChartChallenge | Graphic @cararthompson\nSources: wikipedia.org - ranker.com - rollingstone.com") +
  theme_minimal() %+replace%
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#fbf3dc", colour = "#fbf3dc"),
        plot.background = element_rect(fill = "#fbf3dc", colour = "#fbf3dc"),
        text = element_text(family = "Didot", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 24, colour = "#1a1816", 
                                  family = "Coventry Garden NF"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, colour = "#1a1816", 
                                     face = "plain", family = "Didot",lineheight = 1.1),
        plot.caption = element_text(hjust = 0.5, size = 8, colour = "#1a1816", 
                                    face = "plain", family = "Didot"))

## Export plot ----
ggsave(filename = file.path("../plots", "3.5_pop-culture.png"), 
       dpi = 400, width = 12, height = 7)

## Full links to sources ----
# https://www.ranker.com/list/true-stories-behind-fleetwood-mac-rumours-album/melissa-sartore
# https://en.wikipedia.org/wiki/Lindsey_Buckingham
# https://en.wikipedia.org/wiki/Fleetwood_Mac
# https://www.rollingstone.com/music/music-news/broken-chain-a-history-of-fleetwood-mac-firings-and-departures-628871/