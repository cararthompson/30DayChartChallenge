# Distribution - Physical

## Load libraries ----
library(tidyverse)
library(extrafont)
library(cowplot)
library(ggExtra)

## Read in data ----
penguins <- palmerpenguins::penguins %>%
  mutate(species = gsub("Adelie", "Adélie", species))

# Create label data
labels <- tibble(species = c("Chinstrap", "Adélie", "Gentoo"),
                 species_x = c(190, 205, 228),
                 species_y = c(56, 38, 43))

## Create colour scheme and theme ----
penguin_hues <- c("#d2dbe4", "#8a5d24", "#646376", "#192029", "#acb3bf", "#596e94")

theme_penguin_light <- function() {
  theme_minimal() %+replace%
    theme(text = element_text(colour = penguin_hues[4], family = "Arvo"), 
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.ticks = element_blank())
}

## Plot it ----
p <- ggplot(penguins) +
  scale_colour_manual(values = c(penguin_hues[c(3, 2, 5)]),
                      labels = c("Adélie", "Chinstrap", "Gentoo")) +
  geom_point(aes(x = flipper_length_mm, 
                 y = bill_length_mm, 
                 colour = species,
                 size = body_mass_g),
             alpha = 0.7) +
  geom_text(data = labels,
            aes(x = species_x, y = species_y, 
                label = species, colour = species),
            family = "Arvo", size = 4.5) +
  labs(x = "Flipper length (mm)\n\n\n",
       y = "Bill length (mm)") +
  scale_size_continuous(name = "Body mass (g)") +
  guides(colour = F, size = F) +
  theme_penguin_light()

marg <- ggMarginal(p, type = "densigram", groupColour = T, groupFill = T, alpha = 0.7)

bm <- ggplot(penguins, aes(x = body_mass_g, 
                           fill = species, colour = species)) +
  geom_histogram(aes(y = stat(count)),
                 binwidth = 100,
                 alpha = 0.7,
                 show.legend = F) +
  labs(y = "Count",
       x = "Body mass (g)") +
  scale_fill_manual(values = c(penguin_hues[c(3, 2, 5)]),
                    labels = c("Adélie", "Chinstrap", "Gentoo")) +
  scale_colour_manual(values = c(penguin_hues[c(3, 2, 5)]),
                      labels = c("Adélie", "Chinstrap", "Gentoo")) +
  theme_penguin_light()

title <- ggdraw() +
  draw_label("Palmer Penguins",
             fontfamily = "Arvo",
             colour = penguin_hues[6],
             hjust = 0.5,
             size = 20) 

subtitle <- ggdraw() +
  draw_label("Comparing flipper length, bill length and body mass distributions\nacross all three species in the dataset.\n\n",
             fontfamily = "Arvo",
             colour = penguin_hues[4],
             hjust = 0.5,
             size = 12) 

caption <- ggdraw() +
  draw_label("\n#30DayChartChallenge | Graphic: @cararthompson | Source: {palmerpenguins}",
             fontfamily = "Arvo",
             colour = penguin_hues[4],
             hjust = 0.5,
             size = 8) 

combined_p <- plot_grid(title, 
                        subtitle,
                        marg,
                        bm,
                        caption,
                        ncol = 1, 
                        rel_heights = c(0.05, 0.1, 0.6, 0.2, 0.05))

## Export plot ----
ggsave(plot = combined_p, filename = file.path("../plots", paste0("2.2_animals.png")), 
       dpi = 400, height = 10, width = 8)
