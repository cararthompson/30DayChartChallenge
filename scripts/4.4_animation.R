# Time series - Downwards

## Load libraries ----
library(tidyverse)
library(extrafont)
library(gganimate)
library(ggtext)

## Create data ---
frodo_speed <- 
  # distance in km
  (1800*1.609) /
  # total days - rest days
  ((185 - (28 + 64)) * 
     # assuming 8 hours walking a day
     (8*60))

bilbo_speed <- 
  # distance in km
  (950*1.609) /
  # total days - rest days
  ((172 - (16 + 27 + 1 + 26 + 1)) * 
     # assuming 7 hours walking a day
     (8*60))

racing_df <- tibble(who = rep(c("me-p", "toddler", "frodo", "bilbo", "ben-w", "me-n"), 151),
                    speed = rep(c(1.03 / 16, 
                                  .91 / 37, 
                                  frodo_speed,
                                  bilbo_speed,
                                  14 / 103,
                                  4.84 / 65), 151)) %>%
  mutate(time = sort(rep(c(0:150), 6)),
         distance = speed * time,
         max_dist = case_when(distance < 3 ~ distance,
                              TRUE ~ 3))

labels_df <- tibble(who = c("me-p", "toddler", "frodo", "bilbo", "ben-w", "me-n"),
                    label = c("**Me, 39 weeks pregnant, pushing a buggy** containing said toddler up a steady incline",
                              "**Our toddler**, who likes to stop to point at ants and pick up leaves",
                              "**Frodo Baggins** on his way from the Shire to Mount Doom",
                              "**Bilbo Baggins** on his way from the Shire to the Lonely Mountain",
                              "**Victoria Wilkinson**, the fastest woman to run up and down Ben Nevis",
                              "**My normal** leisurely pace"))

race <- ggplot(racing_df, aes(x = max_dist, y = who)) +
  geom_segment(aes(x = 0, xend = max_dist, y = who, yend = who, colour = who),
               alpha = 0.8, size = 2, show.legend = F) +
  geom_hline(yintercept = seq(1.5, length(unique(labels_df$who)) - 0.5, 1), 
             lwd = 1, colour = "white") +
  geom_vline(xintercept = 3.005, lwd = 2, colour = "#d8330e") +
  geom_vline(xintercept = 3, lwd = 2, colour = "white") +
  geom_vline(xintercept = -.45, lwd = 3, colour = "white") +
  geom_vline(xintercept = 1, lwd = .5, colour = "white", linetype = 2) +
  geom_vline(xintercept = 2, lwd = .5, colour = "white", linetype = 2) +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
                     labels = c(0, 1, 2, 3),
                     limits = c(-1.7, 3.2)) +
  geom_text(data = labels_df, aes(x = 0, y = c(1, 2, 3, 4, 5, 6), 
                                  label = rep("|", 6)),
            family = "Arvo",
            colour = "white", 
            size = 10) +
  geom_point(aes(fill = who), shape = 21,
             colour = "#d8330e",
             show.legend = F, size = 4) + 
  scale_fill_manual(values = c("#7a99b8", 
                               "#6e694e", 
                               "#62472a", 
                               "#9cc9c9", 
                               "#7ab8a8", 
                               "#997ab8")) + 
  scale_colour_manual(values = c("#7a99b8", 
                               "#6e694e", 
                               "#62472a", 
                               "#9cc9c9", 
                               "#7ab8a8", 
                               "#997ab8")) + 
  labs(title = "\n\nNo wonder some books are really long!\n",
       subtitle = "\n\nA 3 km race based on average paces in recorded and written travels*\n\n\n",
       y = "",
       x = "",
       caption = "*The LOTR paces are based on total number of travelling days,\nwith an assumption of 8 hours of walking per day\n\n#30DayChartChallenge | Graphic: @cararthompson\nSources: lotrproject.com/timedistance, wikipedia.org and my Garmin") +
  transition_time(time) +
  geom_textbox(data = labels_df, aes(x = -1.2, y = who, label = label),
               family = "Lato",
               size = 3,
               fill = "#f47152",
               colour = "white",
               box.colour = "#f47152") +
  geom_text(data = labels_df, aes(x = -0.2, y = c(1, 2, 3, 4, 5, 6), 
                                  label = c(1, 2, 3, 4, 5, 6)),
            family = "Arvo",
            colour = "white", 
            size = 12,
            angle = -90) +
  theme_minimal() %+replace%
  theme(panel.background = element_rect(fill = "#f47152", colour = "#f47152"),
        text = element_text(colour = "#36413d"),
        plot.title = element_text(hjust = 0, family = "Arvo", size = 18),
        plot.subtitle = element_text(hjust = 0, family = "Lato", size = 14),
        panel.grid = element_blank(),
        axis.text = element_blank())

## Export plot ----
anim_save(animate(race, nframes = 151, width = 9, height = 6.5, units = "in", res = 300), 
          filename = "../plots/4.4_animation.gif",
          end_pause = 10, rewind = F, fps = 15)
