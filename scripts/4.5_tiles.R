# Time series - Tiles

## Load libraries ----
library(tidyverse)
library(extrafont)
library(rvest)
library(ggtext)

## Get data and tidy it ---
html <- read_html("https://fr.wikipedia.org/wiki/Liste_des_r%C3%A9gimes_politiques_de_la_France") %>%
  html_element("table") %>%
  # Need to reset headers, as there are duplicates in the original
  html_table(trim = T, header = F) 

# To get lubridate to work with French dates
Sys.setlocale("LC_TIME", "French")

regime_df <- html %>%
  select(X1, X2, X3) %>%
  rename(Nom = X1,
         Date = X2, 
         Type = X3) %>%
  filter(!grepl("Nom|Monarchie f", Nom)) %>%
  mutate(start_date_text = gsub("(du )(.*)( à.*| au.*)", "\\2", Date),
         end_date_text = gsub("(.*)( à| au )(.*)(\\[.*)?", "\\3", Date) %>%
           gsub("[4]", "", ., fixed = T) %>%
           gsub("actuel", "22 avril 2021", .)) %>%
  mutate(y_start = 0, 
         y_end = 2) %>%
  select(!Date) %>%
  rbind(
    # Vichy
    tibble(Nom = c("Régime de Vichy", "Commission gouvernementale de Sigmaringen"),
           Type = c("Dictature pluraliste", "Pas de type"),
           start_date_text = c("10 juillet 1940", "6 septembre 1944"),
           end_date_text = c("20 août 1944", "22 avril 1945"),
           y_start = 1,
           y_end = 2)) %>%
  mutate(type_col = case_when(grepl("[ei]mp", Type) ~ "Empire",
                              grepl("Monarchie", Type) ~ "Monarchie",
                              grepl("République", Nom) ~ "République",
                              TRUE ~ "Autre"),
         start_date = lubridate::dmy(start_date_text),
         end_date = lubridate::dmy(end_date_text), 
         Type = gsub("pas de", "Pas de", Type)) %>%
  filter(!is.na(start_date)) %>%
  distinct(Nom, start_date, end_date, .keep_all = T) %>%
  mutate(date_diff = end_date - start_date)

## Plot it
bleu <- "#48577e"
blanc <- "#eed8b3"
rouge <- "#ac2b31"
vichy <- "#4d4445"

ggplot(regime_df) +
#  hacky way of getting circles in legend, as override.aes() doesn't seem to work
  geom_point(data = NULL, aes(x = regime_df$start_date[2] - 3000, y = 1,
                              colour = type_col), size = 8) +
  scale_colour_manual(values = c(bleu, rouge, blanc, vichy),
                    breaks = c("Monarchie", "République", "Empire", "Autre")) +
  geom_rect(aes(xmin = start_date, xmax = end_date,
                ymin = y_start, ymax = y_end,  fill = type_col), 
            colour = "white", show.legend = F) +
  scale_fill_manual(values = c(bleu, rouge, blanc, vichy),
                    breaks = c("Monarchie", "République", "Empire", "Autre")) +
  labs(title = "Douce France, 
cher pays de mon enfance,
qui, depuis la Renaissance,
des régimes en as vu tant !\n",
x = "", y = "",
       caption = "\n\n\n#30DayChartChallenge | Graphic: @cararthompson | Source: fr.wikipedia.org") +
  theme_minimal() %+replace%
  theme(aspect.ratio = 6/200,
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 18, family = "Homemade Apple", lineheight = 1.5),
        text = element_text(family = "Georgia"),
        plot.caption = element_text(hjust = 0.5, size = 8, family = "Georgia"),
        legend.box.spacing = unit(0.5, "cm"),
        legend.title=element_blank()) 
    
## Export plot ----
ggsave(filename = file.path("../plots", "4.5_tiles.png"), 
       dpi = 400, width = 15, height = 5)
