# Distributions - Statistics

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)

# pink: #e91a89
# yellow: #f9f000
# blue: #008eb9

rainfall <- read.csv("https://www2.sepa.org.uk/rainfall/api/Month/15201?csv=true",
                     header = T) %>%
  # Removing the two months on the extremities (Apr 2021 incomplete)
  # and March 2021 to make exactly 10 years worth of data
  filter(!Timestamp %in% c("Feb 2011", "Mar 2021", "Apr 2021")) %>%
  separate(Timestamp, into = c("month", "year"), remove = F) %>%
  # Get month factor levels in right order
  mutate(month = factor(month, levels = month.abb)) %>%
  group_by(month) %>%
  summarise(mean_rainfall = mean(Value)) %>%
  mutate(month_col = case_when(month == "Apr" ~ "Apr",
                               month == "Aug" ~ "Aug",
                               TRUE ~ "Other"))

ggplot(rainfall) +
  geom_col(aes(x = month, y = mean_rainfall, colour = month_col),
           fill = "#27b2db", size = 1.1, show.legend = F) +
  scale_colour_manual(values = c("#f9f000", "#e91a89", "white")) +
  geom_hline(yintercept = filter(rainfall, month == "Apr")$mean_rainfall,
             linetype = 3, size = 1.1, colour = "#f9f000") +
  geom_hline(yintercept = filter(rainfall, month == "Aug")$mean_rainfall,
             linetype = 3, size = 1.1, colour = "#e91a89") +
  # add a base to the histogram
  geom_hline(yintercept = 0,
             linetype = 1, size = 1.1, colour = "white") +
  coord_polar() +
  ylim(c(-100, NA)) +
  labs(x = "",
       y = "",
       title = "\nHow drookit is Auld Reekie?",
       subtitle = "<br>Mean monthly rainfall in Edinburgh from 2011 to 2021, 
       <br>measured at the Edinburgh Royal Botanic Gardens. 
       <br><br><span style='color:#f9f000'>**April**</span> is the driest month on average,
       with a mean total<br>monthly rainfall of 35.76 mm. Meanwhile, 
       <span style='color:#e91a89'>**August**</span>, when
       <br>the world descends on Edinburgh
       for the Fringe and the<br>International Festival, is the second wettest month with 
       <br>more than twice as much rain (mean = 76.12 mm).
       <br><br>The warm welcome more than makes up for the dreich
       <br>weather. Haste ye back!",
       caption = "#30DayChartChallenge | Graphic: @cararthompson | Source: www2.sepa.org.uk/rainfall\n") +
  theme_minimal() %+replace%
    theme(panel.grid = element_blank(),
          panel.background = element_rect(colour = "#008eb9", fill = "#008eb9"),
          plot.background = element_rect(colour = "#008eb9", fill = "#008eb9"),
          text = element_text(colour = "white", family = "Dax-Regular"),
          plot.title = element_text(hjust = 0, size = 20, family = "Dax-Black"),
          plot.subtitle = element_markdown(hjust = 0, size = 16, 
                                           lineheight = 1.2),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12),
          plot.caption = element_text(size = 10, hjust = 0.5)
  )


## Export plot ----
ggsave(filename = file.path("../plots", "2.5_circular.png"), 
       dpi = 400, width = 6, height = 10)
