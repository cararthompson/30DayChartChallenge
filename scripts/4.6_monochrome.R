# Time series - Monochrome

## Load libraries ----
library(tidyverse)
library(extrafont)
library(tuneR)
library(cowplot)

## Read in midi file ----
jsb <- readMidi("../data/midi/bach_847.mid") 

jsb_df <- prelude %>%
  filter(event == "Note On") %>%
  mutate(quantized_time = round(time/10)*10,
         piece = case_when(quantized_time < 72960 ~ "Prelude",
                           TRUE ~ "Fugue")) 

## Create plot elements ----
prelude <- ggplot(filter(jsb_df, piece == "Prelude")) +
  geom_point(aes(x = quantized_time, y = parameter1, 
                 alpha = parameter2),
             show.legend = F) +
  theme_void() %+replace%
  theme(aspect.ratio = 4/20)

fugue <- ggplot(filter(jsb_df, piece == "Fugue")) +
  geom_point(aes(x = quantized_time, y = parameter1, 
                 alpha = parameter2),
             show.legend = F) +
  theme_void() %+replace%
  theme(aspect.ratio = 4/20)

prelude_title <- ggdraw() +
  draw_label("\nBWV 847\n~\n\nPRAELUDIUM II ",
             fontfamily = "Georgia",
             hjust = 0.5, size = 20) 

fugue_title <- ggdraw() +
  draw_label("FUGA II",
             fontfamily = "Georgia",
             hjust = 0.5, size = 20) 

signature <- ggdraw() + 
  draw_image("../making-of/bach_signature.png", scale = 1) 

caption <- ggdraw() +
  draw_label("#30DayChartChallenge | Graphic: @cararthompson | Source: www.piano-midi.de",
             fontfamily = "Georgia",
             hjust = 0.5, size = 10)

## Assemble and export ----
plot_grid(NULL, 
          signature,
          prelude_title,
          prelude,
          fugue_title,
          fugue,
          caption,
          ncol = 1,
          rel_heights = c(0.05, 0.15, 0.08, 0.3, 0.08, 0.3, 0.05))

ggsave(filename = file.path("../plots", "4.6_monochrome.png"), 
       dpi = 400, width = 18, height = 10)
