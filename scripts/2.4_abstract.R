# Distributions - Statistics

## Load libraries ----
library(tidyverse)
library(tidytext)
library(extrafont)
library(ggtext)
library(rvest)
library(cowplot)

kandinsky_text <- read_html("http://www.kandinskypaintings.org/untitled-first-abstract-watercolor/") %>%
  html_elements("h2") %>%
  html_text() %>%
  enframe() 

char.freq <- function(x){
    textdf <- enframe(unlist(strsplit(x, split = NULL))) %>%
      mutate(total_char = length(name)) %>%
      group_by(value) %>%
      summarise(rel_freq = length(name)/total_char,
                rel_position = mean(name, na.rm = T)/total_char) %>%
      distinct() %>%
      filter(value != " ")
  }

set.seed(123)
text_df <- char.freq(kandinsky_text$value) %>%
  mutate(family = sample(c("Potra Light", "Typoliner-RW Light", 
                           "Georgia", "Arvo", "VTF Mixo"), replace = T, length(value)))

kandinsky_pal <- c("#e6d6b5", # background
                  "#333344", "#a43a37", "#b38449", 
                  "#908791", "#95a99c", "#619e6c")
  
# Plot it
painting <- ggplot(text_df) +
  geom_text(aes(x = rel_position, y = rel_freq, label = value, family = family), 
            size = sample(c(15, 20, 30), replace = T, nrow(text_df)), 
            alpha = 0.8, 
            colour = sample(kandinsky_pal[2:length(kandinsky_pal)], 
                            replace = T, nrow(text_df))) +
  xlim(c(min(text_df$rel_position) - 0.1,
         max(text_df$rel_position) + 0.05)) +
  ylim(c(min(text_df$rel_freq) - 0.02,
         max(text_df$rel_freq) + 0.02)) +
  theme_void() %+replace%
  theme(panel.background = element_rect(fill = kandinsky_pal[1], 
                                        colour = kandinsky_pal[1]),
        panel.border = element_rect(colour = "#25282e", fill = NA, size=5),
        #aspect ratio of original painting
        aspect.ratio = 4.96 / 6.48)

label <- ggplot() +
  geom_textbox(aes(x = 1.5, y = 0, 
                label = "**Abstract Distribution** | C. R. Thompson (2021)
                <br>D'apres W. Kandinsky *Untitled (First Abstract Watercolour)*, 1910
                <br><br>The letters come from the header of 
                kandinskypaintings.org's description
                of Kandinsky's *Untitled (First Abstract Watercolour)*, widely recognised
                as one of the first abstract paintings. Each letter's position
                on the X axis represents is mean position within the text; its
                position on the Y axis represents its relative frequency. The fonts,
                letter sizes and colours were randomly generated, the latter being inspired
                by the orignal painting.
                <br><br><br><br>#30DayChartChallenge <br>Graphic: @cararthompson
                <br>Source: kandinskypaintings.org<br>"),
               family = "Segoe UI",
               box.r = unit(0, "pt"),
               width = unit(4, "inch"),
               colour = "#25282e") +
  xlim(c(0, 2)) +
  ylim(c(-0.035, 0.035)) +
  theme_void() 

plot_grid(NULL,
          painting,
          NULL,
          label,
          NULL,
          ncol = 1,
          rel_heights = c(0.025, 0.7, 0.025, 0.375, 0.025))
  
## Export plot ----
ggsave(filename = file.path("../plots", "2.4_abstract.png"), 
       dpi = 400, width = 10.5, height = 12.5)
