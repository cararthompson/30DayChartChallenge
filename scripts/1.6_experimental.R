# Comparisons - Experimental

## Load libraries
library(tidyverse)
library(ggbeeswarm)
library(extrafont)
library(gganimate)

## Create Stroop data
stroop <- tibble(word = sample(c(sample(c("Red", "Green", "Blue", "Orange", "Pink", "Purple", "Yellow"), 
                                        20, prob = rep(1/7, 7), replace = T),
                                 sample(c("Backpack", "Mirror", "Piano", "Chair", "Book", "Table", "Tap"), 
                                        20, prob = rep(1/7, 7), replace = T),
                                 sample(c("Sandwich", "Chocolate", "Chips", "Pasta", "Burger", "Cake", "Pizza"), 
                                        20, prob = rep(1/7, 7), replace = T)), 60),
                 colour = sample(c("#c32d38", "#e16fa0", "#036da7", "#df533a", "#f0c227", "#00a24d", "#43346d"), 60, 
                                 prob = rep(1/7, 7), replace = T),
                 word_progress = c(1:60))

diff <- tibble(difficulty_x = c(0.5, 5, 9.5),
               type = c("Colour", "Object", "Food"),
               difficulty_min = c(10.5, 2, 2),
               difficulty_max = c(10.5, 2, 9.5))


## Plot it
for(progress in stroop$word_progress) {
  p <- ggplot() +
    geom_line(data= diff, aes(x = difficulty_x, y = difficulty_max), 
              linetype = 3, size = 1, alpha = 0.5) +
    geom_line(data= diff, aes(x = difficulty_x, y = difficulty_min),
              linetype = 3, size = 1, alpha = 0.8) +
    geom_point(data = diff, aes(x = difficulty_x, y = difficulty_max), size = 4, alpha = 0.5) + 
    geom_point(data = diff, aes(x = difficulty_x, y = difficulty_min), size = 4, alpha = 0.5) + 
    geom_text(data = filter(stroop, word_progress == progress),
              aes(x = 5, y = 5, label = word, 
                  colour = colour),
              size = 30, family = "Arvo",
              show.legend = F) +
    scale_colour_manual(values = filter(stroop, word_progress == progress)$colour) +
    geom_text(data = NULL, 
              aes(x = 9.5, y = 5.75, angle = 90, 
                  label = "How hungry are you...?"), alpha = 0.8,
              family = "Arvo", hjust = 0.5, vjust = 0.3, size = 4) +
    annotate("curve", x = -0.5, xend = -0.5, y = 0, 
             yend = 11, curvature = 0, size = 2,
             arrow = arrow(length = unit(3, "mm"))) + 
    annotate("curve", x = 9.5, xend = 9.5, y = 3.5, 
             yend = 2.5, curvature = 0, size = 1, alpha = 0.8,
             arrow = arrow(length = unit(2, "mm"))) + 
    annotate("curve", x = 9.5, xend = 9.5, y = 8, 
             yend = 9, curvature = 0, size = 1, alpha = 0.8,
             arrow = arrow(length = unit(2, "mm"))) +
    scale_x_continuous(breaks = c(0.5, 5, 9.5),
                       labels = c("Colours", "Random Objects", "Food")) +
    scale_y_continuous(breaks = c(0.5, 10),
                       labels = c("Easy peasy", "Really hard")) +
    labs(title = "The Stroop Effect",
         subtitle = "\nWhat colour is the text?",
         y = "Difficulty level",
         x = "") +
    theme_minimal() %+replace%
    theme(text = element_text(family = "Arvo", size = 16),
          panel.grid = element_blank(),
          axis.text.y = element_text(angle = 0, colour = "black"),
          axis.text.x = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 24, lineheight = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 20))
  
  
  ## Export plots to make gif
  ggsave(plot = p, filename = file.path("../making-of", paste0("1.6_", progress, "_experimental.png")), 
         dpi = 400, width = 10, height = 7)
}
