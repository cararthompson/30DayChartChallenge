# Relationships - Correlation

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)
library(viridis)

## Read in data and plot it ---- 
# Antenatal care coverage for at least four visits
visits <- read.csv("../data/undata/UNdata_Export_20210412_4-antenatal-visits.csv",
                   header = T) %>%
  filter(Unit == "Percent") %>%
  left_join(read.csv("../data/undata/UNdata_Export_20210412_2012-gdps.csv", 
                     header = T), by = "Country.or.Area") %>%
  rename(av_cover = Value.x,
         gdp = Value.y) %>%
  filter(!is.na(gdp)) 

visits <- visits %>%
  mutate(resid = resid(lm(av_cover ~ log10(gdp), data = visits)))

# Add a tibble for outlier labels
outliers <- tibble(
  label_x = c(200, 5000, 45000),
  label_y = c(92, 23, 45),
  text = c("**North Korea** stood out as the country for which the percentage of women offered four or more antenatal visits far exceeded predictions based on GDP alone.",
           "**Yemen**, **Nauru** and **Mauritania** all provided lower antenatal visit cover than predicted, falling more than 40% short of their GDP predictions.", 
           "Although there were very few countries with a high GDP in the dataset, **Italy** stood out as one in which the actual cover (68%) fell far short of what was predicted (close to 100%)"))

## Plot it
ggplot(visits, aes(x = gdp, y = av_cover,
                   label = Country.or.Area)) +
  geom_smooth(method = lm, se = F, 
              size = .5,
              colour = "white", 
              show.legend = F) +
  geom_point(aes(fill = resid,
                 colour = -resid,
                 alpha = abs(resid)),
             size = 4,  
             shape = 21,
             show.legend = F) +
  ylim(c(0, 108)) +
  scale_fill_viridis(option = "A") +
  scale_colour_viridis(option = "A") +
  scale_alpha(range = c(0.3, 1)) +
  scale_x_log10() +
  labs(x = "GDP (log 10)",
       y = "Percentage of mothers-to-be with health coverage\n for four or more antenatal visits\n",
       title = "\nAntenatal visits and GDP - A correlation with notable outliers",
       subtitle = "
<br>This plot combines two different UN datasets, both featuring data from 2012. Since the countries in 
<br>the dataset were heavily skewed towards lower GDPs, the data were not normally distributed, breaking 
<br>one of the assumptions required for a correlation. Instead, we used the logarithm (base 10) of the 
<br>GDP values (*e.g.* log<sub>10</sub>(1000) = 3) in both the plot and the correlation test. Exploring the data this
<br>way showed a significant correlation between GDP and the percentage of women offered four or more 
<br>antenatal visits (*r* = 0.66, *p* < 0.001), with the relationship between GDP and antenatal care cover 
<br>accounting for 43% of the variance in the dataset. 
<br><br>The further away from the linear prediction model a country lies, the more the dot that represents 
<br>it stands out from its background. Countries represented by lighter and darker dots achieved 
<br>respectively higher and lower levels of antenatal cover than what would be predicted from their GDP 
<br>alone.
<br><br>Notable outliers are highlighted in the plot.",
caption = "\n#30DayChartChallenge | Graphic: @cararthompson | Source: UNData") +
  geom_textbox(data = outliers, aes(x = label_x, y = label_y, label = text), 
               colour = "white",
               box.colour = "#022531",
               fill = NA, 
               family = "Segoe UI") +
  annotate("curve", x = 425, xend = 595, y = 100, yend = 96, curvature = -.2, 
           size = .4, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  annotate("curve", x = 46000, xend = 38000, y = 59, yend = 67, curvature = .25, 
           size = .4, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  annotate("curve", x = 2300, xend = 1800, y = 28.5, yend = 18.5, curvature = .2,
           size = .4, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  annotate("curve", x = 2300, xend = 1300, y = 31.5, yend = 16.5, curvature = .2,
           size = .4, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  annotate("curve", x = 5000, xend = 10000, y = 34, yend = 40, curvature = -.2,
           size = .4, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  theme_minimal() %+replace%
  theme(plot.background = element_rect(colour = "#022531",
                                       fill = "#022531"),
        panel.background = element_rect(colour = "#022531",
                                       fill = "#022531"),
        panel.grid = element_line(linetype = 3, colour = "#044a62", size = 0.2),
        text = element_text(colour = "white", family = "Segoe UI"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        plot.subtitle = element_markdown(lineheight = 1.25, hjust = 0, size = 13),
        axis.text = element_text(colour = "white"))

## Export plot ----
ggsave(filename = file.path("../plots", "3.1_correlation.png"), 
       dpi = 400, width = 10, height = 11)
