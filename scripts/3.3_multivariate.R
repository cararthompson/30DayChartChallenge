# Relationships - Multivariate

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggbeeswarm)
library(cowplot)
library(patchwork)

## Read in and rework data ----
athletes <- read.csv("../data/kaggle/rio-2016/athletes.csv") %>%
  filter(sport %in% c("gymnastics", 
                      "cycling")) %>%
  mutate(at_least_one = as.factor(case_when(gold > 0 ~ 1,
                                  TRUE ~ 0)),
         wh_ratio = weight / height,
         sex = case_when(sex == "female" ~ "Female",
                         sex == "male" ~ "Male"),
         sport = case_when(sport == "cycling" ~ "Cyclists",
                           sport == "gymnastics" ~ "Gymnasts"))

## The models I explored
# summary(glm(gold ~ weight, data = athletes))
# summary(glm(at_least_one ~ weight, data = athletes, family = "binomial"))
# summary(glm(gold ~ weight*sport*sex, data = athletes))
# summary(glm(at_least_one ~ weight*sport*sex, data = athletes, family = "binomial"))

## Build theme ----
olympic_pal <- list(gold = "#f1b346",
                    red = "#e73654",
                    green = "#308448",
                    blue = "#0d84c2",
                    black = "#020202")

theme_olympic <- function() {
  theme_minimal() %+replace%
    theme(plot.title = element_text(hjust = 0.5, size = 16, colour = olympic_pal$black, 
                                    family = "Georgia", face = "bold"),
          plot.subtitle = element_text(hjust = 0, size = 12, colour = olympic_pal$black, 
                                       family = "Segoe UI", lineheight = 1.05),
          axis.text = element_text(colour = olympic_pal$black,
                                   family = "Segoe UI"),
          axis.title = element_text(colour = olympic_pal$black,
                                   family = "Segoe UI"),
          strip.text = element_text(colour = olympic_pal$black,
                                    family = "Segoe UI"),
          legend.position = "none")
}


## Create plots ----
weight <- ggplot(athletes, aes(x = 1, y = weight)) +
  scale_colour_manual(values = c("#2b2b2b", olympic_pal$gold)) +
  geom_quasirandom(aes(colour = at_least_one, size = gold), alpha = 0.9) +
  labs(x = "",
       y = "Weight (kg)",
       title = "Gold medals by weight\n",
       subtitle = "Our first plot illustrates that weight per se is not 
a predictor of medal success. Here, we see that the 
gold medalists are pretty evenly distributed across 
athletes of different weights. So, on a surface level, 
the answer to our question is \"No\".") +
  theme_olympic() %+replace%
  theme(axis.text.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

weight_sport <- ggplot(athletes, aes(x = sport, y = weight)) +
  geom_quasirandom(aes(colour = at_least_one, size = gold), alpha = 0.9) +
  scale_colour_manual(values = c("#2b2b2b", olympic_pal$gold)) +
  labs(title = "Gold medals by weight and sport\n",
       subtitle = "In our model, there was no significant interaction 
effect of weight and sport in predicting gold medal
success. This is illustrated below in the fact that, 
while cyclist tended to be heavier than gymnasts, 
the medal distribution within each group is still
pretty even.",
       y = "Weight (kg)",
       x = "") +
  theme_olympic()

weight_sport_sex <- weight_sport +
  labs(title = "Gold medals by weight, sport and sex\n",
       subtitle = "
In our final plot, we come to our only significant predictor of gold medal success involving weight: a weight x
sport x sex interaction. This effect is illustrated in the fact that the more successful male cyclists tended to
be on the heavier end of the male cyclists, while the more successful male gymnasts tended to be on the lighter
end of the male gymnasts Note that this weight pattern is not observed in female athletes. So here we have it:
weight is only a significant predictor of gold medal success in combination with sport and sex.
") +
  facet_wrap(~sex)

## Add a bit of olympic colour and combine components for the final layout ----
olympic_image <- ggdraw() + 
  draw_image("../making-of/3.3_olympic.png", scale = 1) 

red_line <- ggdraw() +
  draw_line(
    x = c(0.1, 0.9),
    y = c(0.3, 0.3),
    color = olympic_pal$red, size = 2
  )

blue_line <- ggdraw() +
  draw_line(
    x = c(0.1, 0.9),
    y = c(0.3, 0.3),
    color = olympic_pal$blue, size = 2
  )

green_line <- ggdraw() +
  draw_line(
    x = c(0.2, 0.8),
    y = c(0.3, 0.3),
    color = olympic_pal$green, size = 2
  )

title <- ggdraw() +
  draw_label("\nIs weight a predictor of gold medal success?\n",
             fontfamily = "Georgia",
             fontface = "bold",
             colour = olympic_pal$black,
             hjust = 0.5,
             size = 24) 

subtitle <- ggdraw() +
  draw_label("Using data relating to Cyclists and Gymnasts in the 2016 Rio Olympic 
Games, we built a model predicting the number of gold medals as a 
function of weight, sport and sex. All just to illustrate the concept of 
a multivariate analysis.

In the plots below, each dot represents an athlete. The gold dots 
represent gold medalists and the black dot represent other athletes. 
The larger the gold dot, the more gold medals the gold medalist won.",
             fontfamily = "Segoe UI",
             colour = olympic_pal$black,
             hjust = 0.5,
             size = 12, lineheight = 1.05) 

caption <- ggdraw() +
  draw_label("#30DayChartChallenge | Graphic: @cararthompson | Source: Kaggle",
             fontfamily = "Georgia",
             colour = olympic_pal$black,
             hjust = 0.5,
             size = 10)

top_row <- plot_grid(olympic_image, subtitle,
                     ncol = 2, rel_widths = c(0.3, 0.7))

p <- title / 
  top_row /
  (red_line | blue_line) /
  (weight | weight_sport) /
  (green_line) /
  weight_sport_sex / 
  caption +
  plot_layout(heights = unit(c(1.5, 4, .5, 4, .5, 4.5, .5), c('cm', 'null')))

## Export plot ----
ggsave(plot = p, filename = file.path("../plots", "3.3_multivariate.png"), 
       dpi = 400, width = 9.5, height = 14.5)
