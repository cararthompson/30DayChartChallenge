# Comparisons - Pictogram

## Load libraries
library(tidyverse)
library(ggtext)
library(extrafont)
library(ggfx)
library(ggimage)


## Read in and reorganise data
gender_in_education <- read.csv("../data/undata/UNdata_Export_20210314_primary-education.csv",
                                header = T, check.names = F)

gender_df <- gender_in_education %>%
  filter(Year == "2005") %>%
  mutate(Value = parse_number(Value)) %>%
  select(`Country or Area`, `Year`, `Subgroup`, `Value`) %>%
  pivot_wider(id_cols = c(`Country or Area`, `Year`), 
              names_from = Subgroup, values_from = Value) %>%
  mutate(prop_female = Female / (Female + Male) * 100,
         prop_male = Male / (Female + Male) * 100) %>%
  filter(prop_female %in% quantile(prop_female, 
                                   probs = seq(0, 1, 0.1), type = 1)) %>%
  arrange(prop_female) %>%
  mutate(rank = c(0:10)) %>%
  pivot_longer(cols = c(prop_female, prop_male)) %>%
  mutate(gender = factor(name, levels = c("prop_male", "prop_female")))

## Create theme
theme_light_void <- function() {
  theme_void() %+replace%
    theme(text = element_text(colour = "#393d3b", family = "Segoe UI", size = 14), 
          plot.title = element_markdown(hjust = 0.5, size = 20, colour = "#393d3b", family = "Segoe UI"),
          plot.subtitle = element_text(hjust = 0.5, size = 14, colour = "#393d3b", family = "Segoe UI"),
          axis.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = "#393d3b", size = 10, family = "Segoe UI"),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

pupils <- data.frame(x_pupil = sort(rep(c(0:10), 11)),
                     y_pupil = rep(seq(0, 100, 10), 11))

icon <- "../making-of/student-icon.png"

# Plot it!
ggplot() +
  geom_text(data = gender_df,
            aes(y = -2, x = rank, label = `Country or Area`), 
            hjust = 1, colour = "#393d3b", family = "Segoe UI") +
  as_reference(
    geom_image(data = pupils, aes(x = x_pupil, y = y_pupil, image = icon), 
               size = 0.08),
    id = "text"
  ) +
  with_blend(
    geom_bar(data = gender_df, aes(fill=name, y=value, x=rank),
             position = position_stack(reverse = T),
             stat="identity", show.legend = F),
    bg_layer = "text",
    blend_type = 'in'
  ) +
  ylim(-10, 110) +
  scale_fill_manual(values = c("#703287", "#328761")) +
  geom_hline(yintercept = 50, colour = "#393d3b", 
             linetype = 2, size = 1) +
  theme_void() +
  coord_flip() +
  labs(title = "<br><span style='color:#703287'>Girls</span> and <span style='color:#328761'>Boys</span> in Classrooms around the World<br>",
       subtitle = "The countries in this plot are those which fell on each 10th percentile of\nproportion of female pupils in the most recent year in the dataset (2005).\n\n\n50%",
       caption = "\n\n#30DayChartChallenge | Graphic: @cararthompson | Source: UNdata\n") +
  theme_light_void()


## Export plot ----
ggsave(filename = file.path("../plots", "1.2_pictogram.png"), 
       dpi = 400, width = 8, height = 9)
