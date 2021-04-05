# Comparisons - Slope

## Load libraries
library(tidyverse)
library(extrafont)
library(rvest)
library(ggrepel)


# Read in data

## Countries in Europe: https://www.worldometers.info/geography/how-many-countries-in-europe/
european_countries <- read_html("https://www.worldometers.info/geography/how-many-countries-in-europe/") %>%
  html_elements(xpath = "//*[@id=\"example2\"]") %>%
  html_table()
european_tibble <- european_countries[[1]]

## Internet access: UNdata
internet_access <- read.csv("../data/undata/UNdata_Export_20210314_internet-access-by-country-date.csv",
                             header = T, check.names = F) %>%
  # 1990 is most complete early year in dataset; 2014 is most recent
  filter(Year %in% c("1990", "2014")) %>%
  rename(Country = `Country or Area`) %>%
  mutate(Value = parse_number(Value)) %>%
  group_by(Country) %>%
  add_count() %>%
  # keep only countries that have a value for both 1990 and 2014
  filter(n == 2) %>%
  select(c(Country, Year, Value)) %>%
  inner_join(european_tibble)
  
  
## Create theme
theme_slope <- function() {
  theme_minimal() %+replace%
    theme(plot.title = element_text(hjust = 0, size = 14, colour = "#36413d", 
                                    family = "Arvo"),
          plot.subtitle = element_text(hjust = 0, size = 10, colour = "#36413d", 
                                    family = "Arvo"),
          axis.text = element_text(color = "#36413d", size = 8, 
                                   family = "Arvo"),
          legend.position = "bottom",
          legend.justification = c(.5, .5),
          legend.text = element_text(color = "#36413d", size = 8, 
                                     family = "Arvo"),
          plot.caption = element_text(color = "#36413d", size = 8, hjust = 1,
                                     family = "Arvo"))
}

# https://personal.sron.nl/~pault/#fig:orbits
cb_palette <- c("#117733", "#CC6677", "#882255", "#44AA99")

# Plot it!
 ggplot(internet_access,
        aes(x = Year, y = Value, group = Country)) +
   geom_line(aes(colour = Subregion, size = 2, alpha = 0.8),
             show.legend = F) +
   geom_point(aes(colour = Subregion, size = 2.5), 
              alpha = 0.7,
              show.legend = F) +
   geom_text_repel(data = internet_access %>%
                     filter(Year == "2014"), 
                   aes(label = paste0(Country, " - ", round(Value), "%"),
                       colour = Subregion) ,
                   hjust = "right",
                   family = "Arvo",
                   size = 3,
                   segment.color = 'transparent',
                   nudge_x = 0.55,
                   direction = "y",
                   key_glyph = "point") +
   scale_colour_manual(values = cb_palette) +
   annotate("text", x = "1990", y = 30, 
            label = "In 1990, the proportion of
the population with
access to the internet 
was below 1% in all 
European countries",
            family = "Arvo", colour = "#36413d",
size = 3) + 
   labs(title = "Internet access in Europe: 1990 - 2014",
        caption = "\n#30DayChartChallenge | Graphic: @cararthompson\nSources: UNdata & worldometers.info\n",
        colour = "",
        subtitle = "\nPercentage of population with access to the Internet\n",
        x = "",
        y = "") +
   theme_slope()
 
## Export plot ----
ggsave(filename = file.path("../plots", "1.5_slope.png"), 
       dpi = 400, width = 9, height = 7)

