# Distribution - Physical

## Load libraries ----
library(tidyverse)
library(extrafont)
library(datasauRus)
library(gganimate)
library(ggfx)
library(ggside)
library(ggtext)

df <- datasaurus_dozen %>%
  mutate(data_colour = case_when(dataset == "dino" ~ "dino",
                                 TRUE ~ "other"))

labels <- datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )

pal <- c("#010000", # black
         "#fff033", # yellow
         "#e6000f") #red

## Plot it ----
p <- ggplot(df, aes(x=x, y=y, colour=data_colour))+
  with_outer_glow(geom_point(show.legend = F),
                  colour = "white") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal[3:2]) +
  transition_states(dataset, transition_length = 2, state_length = 4, wrap = T) +
  ease_aes("cubic-in-out") +
  with_outer_glow(geom_xsidehistogram(data = df, aes(y=stat(count), colour = NULL), 
                                      fill = "white", 
                                      show.legend = F), colour = "red") +
  with_outer_glow(geom_ysidehistogram(aes(x=stat(count), colour = NULL), 
                                      fill = "white",
                                      show.legend = F), colour = "red") +
  with_outer_glow(geom_xsideboxplot(aes(y = -40), width = 30, alpha = 0,
                                    orientation = "y",
                                    show.legend = F), colour = "red") +
  with_outer_glow(geom_ysideboxplot(aes(x = -40), width = 30, alpha = 0,
                                    orientation = "x", 
                                    show.legend = F), colour = "red") +
  ggside(x.pos = "bottom") +
  labs(title = "\nBeware ::: hidden dino!",
       subtitle = "<br>\"Your scientists were so preoccupied with<br>whether they could,<br>they didn't stop to think if they should.\"
<br><br>On the value of plotting <span style='font-family:African'>roooaaar</span> data.",
       caption = "\n#30DayChartChallenge | Graphic: @cararthompson | Source: datasauRus\n\n") +
  geom_text(data = labels, aes(x = min(df$x), y = 110, 
                               label = paste0("mean(x) = ", round(mean_x, 2),
                                              " | sd(x) = ", round(std_dev_x, 2))),
            hjust = 0, colour = "white", family = "Raleway") +
  geom_text(data = labels, aes(x = max(df$x), y = 110, 
                               label = paste0("mean(y) = ", round(mean_y, 2),
                                              " | sd(y) = ", round(std_dev_y, 2))),
            hjust = 1, colour = "white", family = "Raleway") +
  geom_text(data = labels, aes(x = mean(c(min(df$x), max(df$x))), y = 110, 
                               label = paste0("| cor(x,y) = ", round(corr_x_y, 2), " | ")),
            hjust = 0.5, colour = "white", family = "Raleway") +
  theme_void() %+replace%
  theme(plot.background = element_rect(fill = pal[1], colour = pal[1]),
        panel.grid = element_line(colour = pal[1]),
        text = element_text(colour = "white", family = "Raleway"),
        plot.title = element_text(family = "African", hjust = 0.5, size = 20),
        plot.subtitle = element_markdown(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0.5, size = 8),
        ggside.panel.scale.x = .2,
        ggside.panel.scale.y = .1)

## Export plot plot ----
anim_save(animate(p),
          filename = file.path("../plots", "2.3_statistical.gif"))
