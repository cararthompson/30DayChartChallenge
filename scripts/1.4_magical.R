# Comparisons - Magical

## Load libraries
library(tidyverse)
library(waffle)
library(extrafont)
library(cowplot)


# Read in data
wc_df <- read.csv("../data/manual-data-collection/lotr-vs-narnia.csv",
                        header = T) %>%
  mutate(wc = round(word_count / 5000),
         # Keep books in order for plotting
         book = factor(book, levels = unique(book)))

## Create theme
theme_waffle <- function() {
  theme_void() %+replace%
    theme(plot.title = element_text(hjust = 0.08, size = 14, colour = "#36413d", 
                                    family = "Arvo"),
          axis.text = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(color = "#36413d", size = 8, 
                                     family = "Arvo"))
}

# Plot it!
lotr <- ggplot(filter(wc_df, opus == "Lord of the Rings")) +
  geom_waffle(aes(fill = book, values = wc),
              colour = "white", size = 1.5, n_rows = 4) +
  labs(title = "Lord of the Rings",
       fill = "") +
  scale_fill_manual(values = c("#aea382", "#62472a", "#6e694e")) +
  coord_equal() +
  theme_waffle() +
  guides(fill=guide_legend(ncol=3)) +  
  # Giving same xlims to align plots with cowplot
  xlim(c(0, 25))

narnia <- ggplot(filter(wc_df, opus == "The Chronicles Of Narnia")) +
  geom_waffle(aes(fill = book, values = wc),
              colour = "white", size = 1.5, n_rows = 4, ) +
  labs(title = "  The Chronicles Of Narnia",
       fill = "") +
  scale_fill_manual(values = c("#303f3a", "#a2c4be", "#735737", "#ddddd0", 
                               "#29747a", "#d09a51", "#c92e30")) +
  coord_equal() +
  theme_waffle() +
  guides(fill=guide_legend(ncol=3)) +
  # Giving same xlims to align plots with cowplot
  xlim(c(0, 25))

title <- ggdraw() +
  draw_label("\nSome books are really long",
             fontfamily = "Arvo",
             colour = "#36413d",
             hjust = 0.5,
             size = 20) 

subtitle <- ggdraw() +
  draw_label("Each square represents 5,000 words",
             fontfamily = "Arvo",
             colour = "#36413d",
             hjust = 0.5,
             size = 16)

caption <- ggdraw() +
  draw_label("\n#30DayChartChallenge | Graphic: @cararthompson\nSource: https://blog.fostergrant.co.uk/2017/08/03/word-counts-popular-books-world/\n",
             fontfamily = "Arvo",
             colour = "#36413d",
             hjust = 0.5,
             size = 8) 

p <- plot_grid(title,
               subtitle,
               lotr,
               narnia,
               caption,
               ncol = 1,
               rel_heights = c(0.1, 0.1, 0.35, 0.35, 0.1))

## Export plot ----
ggsave(plot = p, filename = file.path("../plots", "1.4_magical.png"), 
       dpi = 400, width = 7, height = 7)

