# Comparisons - Historical

## Load libraries
library(tidyverse)
library(rvest)
library(tidytext)
library(ggtext)
library(extrafont)
library(ggwordcloud)
library(cowplot)

set.seed(123)


## Scrape web pages and tidy up as we go (each have slightly different formatting!)
catechisms <- read_html("https://reformedstandards.com/three-forms-of-unity/heidelberg-catechism.html") %>%
  html_elements("h4") %>%
  html_text() %>%
  enframe() %>%
  mutate(clean_text = stringr::str_squish(gsub("Q \\d+: |\\([a-z]\\)|\\[[a-z]\\]", "", 
                                               value, fixed = F)),
         order = 1:length(value), 
         catechism = "heicat") %>%
  bind_rows(
    # heicat answers
    read_html("https://reformedstandards.com/three-forms-of-unity/heidelberg-catechism.html") %>%
      html_elements("p") %>%
      html_text() %>%
      enframe() %>%
      filter(grepl("Answer", value))%>%
      mutate(clean_text = stringr::str_squish(gsub("Answer: |\\([a-z]\\)|\\[[a-z]\\]", "", 
                                                   value, fixed = F)),
             # footnotes
             clean_text = gsub("(\\.|;|,|!)([a-z])", "\\1", clean_text, fixed = F),
             order = 1:length(value), 
             catechism = "heicat")) %>%
  bind_rows(
    # wescat questions
    read_html("https://reformedstandards.com/westminster/wlc.html") %>%
      html_elements("h3") %>%
      html_text() %>%
      enframe() %>%
      mutate(clean_text = stringr::str_squish(gsub("Q \\d+: |\\([a-z]\\)|\\[[a-z]\\]", "", 
                                                   value, fixed = F)),
             order = 1:length(value), 
             catechism = "Wescat")) %>%
  bind_rows(
    # wescat answers
    read_html("https://reformedstandards.com/westminster/wlc.html") %>%
      html_elements("p") %>%
      html_text() %>%
      enframe() %>%
      filter(grepl("Answer", value))%>%
      mutate(clean_text = stringr::str_squish(gsub("Answer: |\\([a-z]\\)|\\[[a-z]\\]", "", 
                                                   value, fixed = F)),
             # footnotes
             clean_text = gsub("(\\.|;|,|!)([a-z]|[0-9+])", "\\1", clean_text, fixed = F),
             order = 1:length(value), 
             catechism = "wescat")) %>%
  # get Q&As in right order with each other
  arrange(catechism, order)


# Get word frequencies
cat_freqs <- catechisms %>%
  unnest_tokens(word, clean_text) %>%
  group_by(catechism) %>%
  count(word, sort = T, name = "word_count") %>%
  anti_join(stop_words) %>%
  # remove remaining numbers
  filter(!word %in% c(0:100),
         word != "16,17")

## Create theme
theme_historic <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#d4bd9d", colour = "#d4bd9d"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#d4bd9d", colour = "#d4bd9d"),
          plot.title = element_text(hjust = 0.5, size = 25, colour = "#985943", 
                                    family = "DejaVu Serif Condensed"),
          axis.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = "#985943", size = 10, 
                                      family = "DejaVu Serif Condensed"),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

# Plot it!
h_plot <- ggplot(data = filter(cat_freqs, catechism == "heicat") %>%
                   top_n(200),
                 aes(label = word, size = word_count)) +
  geom_text_wordcloud_area(
    mask = png::readPNG("../making-of/H.png"),
    rm_outside = TRUE,
    family = "DejaVu Serif Condensed",
    colour = "#594635",
    shape = "square"
  ) +
  scale_size_area(max_size = 18) +
  theme_historic()

w_plot <- ggplot(data = filter(cat_freqs, catechism == "wescat") %>%
                   top_n(200),
                 aes(label = word, size = word_count)) +
  geom_text_wordcloud_area(
    mask = png::readPNG("../making-of/w.png"),
    rm_outside = TRUE,
    family = "DejaVu Serif Condensed",
    colour = "#594635",
    shape = "square"
  ) +
  scale_size_area(max_size = 15) +
  theme_historic()

title <- ggdraw() +
  draw_label("\nTop 200 words in the \nHeidelberg and Westminster Catechisms",
             fontfamily = "DejaVu Serif Condensed",
             colour = "#985943",
             hjust = 0.5,
             size = 24) +
  theme_historic()

caption <- ggdraw() +
  draw_label("\n#30DayChartChallenge | Graphic: @cararthompson | Source: reformedstandards.com\n",
             fontfamily = "DejaVu Serif Condensed",
             colour = "#985943",
             hjust = 0.5,
             size = 8) +
  theme_historic()

p <- plot_grid(title,
          plot_grid(h_plot, w_plot, nrow = 1, rel_widths = c(0.5, 0.5)),
          caption,
          ncol = 1,
          rel_heights = c(0.2, 0.7, 0.1))


## Export plot ----
ggsave(plot = p, filename = file.path("../plots", "1.3_historical.png"), 
       dpi = 400, width = 9.05, height = 7)
