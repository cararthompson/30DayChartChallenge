# Distribution - Physical

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggmap)
library(ggthemes)
library(viridis)

## Read in data ----
pop_density <- read.csv("../data/world-pop/fra_pd_2020_1km_ASCII_XYZ.csv",
                        header = T) %>%
  mutate(Country = "France") %>%
  bind_rows(read.csv("../data/world-pop/gbr_pd_2020_1km_ASCII_XYZ.csv",
                     header = T) %>%
              mutate(Country = "UK")) %>%
  mutate(log10_density = log10(Z))

## Plot it ----
p <- ggplot(pop_density, aes(x = X, y = Y)) +
  geom_point(aes(colour = log10_density), 
             size = 0.01, alpha = 0.4,
             show.legend = F) +
  scale_colour_viridis() +
  coord_map() +
  labs(title = "\"Ici nous sommes!\"*",
       subtitle = "\nPopulation density across the UK and France",
       caption = "* In memory of my grandmother's love of nonsensical literal translations.
\n\n#30DayChartChallenge | Graphic: @cararthompson\nSource: worldpop.org") +
  theme_map() %+replace%
  theme(
    text = element_text(family = "Georgia", colour = "#36413d"),
    plot.title = element_text(hjust = 0, size = 20, lineheight = 5),
    plot.subtitle = element_text(hjust = 0, size = 15),
    plot.caption = element_text(hjust = 1, size = 10)
  )

## Export plot ----
ggsave(plot = p, filename = file.path("../plots", paste0("2.1_physical.png")), 
       dpi = 400, height = 10, width = 6)
