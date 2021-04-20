# Relationships - Connection

## Load libraries ----
library(tidyverse)
library(extrafont)
library(visNetwork)

## Read in and rework data ----
cross_refs <- read.delim("../data/openbibleinfo/cross_references.txt")

cr_df <- cross_refs %>%
  # The 5 Psalms in the series
  filter(grepl("Ps\\.1\\.|Ps\\.62\\.|Ps\\.86\\.|Ps\\.94\\.|Ps\\.145\\.", From.Verse, fixed = F) & 
           grepl("Matt.|Mark.|Luke.|^John.", To.Verse)) %>%
  rename(to = To.Verse,
         from = From.Verse) %>%
  select(from, to) %>%
  mutate(weight = 5,
         level = 3) 

hierarchy <- tibble(to = c(unique(cr_df$from))) %>%
  mutate(from = gsub("(^.*\\..*)(\\...*)", "\\1", to),
         level = 2,
         weight = 10) 

top <- tibble(to = unique(grep("Ps", hierarchy$from, value = T))) %>%
  mutate(from = "Psalms",
         level = 1,
         weight = 20)

links <- cr_df %>%
  rbind(hierarchy) %>%
  rbind(top)

# Add plotting options to nodes dataframe
nodes <- tibble(id = unique(c(links$from, links$to))) %>%
  mutate(chapter = case_when(grepl("Ps.", id) ~ gsub("(^.*\\..*)(\\...*)", "\\1", id),
                             TRUE ~ gsub("([Aa-z])(\\..*)", "\\1", id)),
         overarching = factor(case_when(grepl("Ps.", id) ~ "Psalms",
                                        TRUE ~ "Gospels")),
         shape = "dot",
         shadow = T,
         label = id,
         size = 20,
         borderwidth = 2,
         font.family = "Segoe UI",
         font.size = 30,
         font.color = "#8d96a5",
         color.background = case_when(overarching == "Psalms" ~ "#99c09e",
                                      overarching == "Gospels" ~ "#cf96aa"),
         color.border = "#656f80",
         color.highlight = case_when(overarching == "Psalms" ~ "#71a878",
                                  overarching == "Gospels" ~ "#bf738e"),
         color.highlight.border = "#656f80")

## Plot it ----
abide <- visNetwork(nodes, links, width="100%", height="900px", 
           main = list(text = "<b>A B I D E&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;April 2021<b>",
                       style = "font-family:Segoe UI;color:#656f80;font-size:30px;font-face:bold;text-align:center;"),
           submain = list(text = "Finding Psalms 1, 62, 86, 94 and 145 in the Gospels",
                       style = "font-family:Segoe UI;color:#656f80;font-size:24px;text-align:center;"),
           footer = list(text = "#30DayChartChallenge | Graohic: @carathompson | Source: www.openbible.info",
                         style = "font-family:Segoe UI;color:#656f80;font-size:12px;text-align:center;")) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F),autoResize = T) %>%
  visInteraction(hover = T, selectConnectedEdges = T, dragNodes = T, dragView = T, 
                 zoomView = F) %>%
  visIgraphLayout(physics = T, randomSeed = 123) %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -300),
             stabilization = F) %>%
  visEvents(type = "once", beforeDrawing = "function() {
            this.moveTo({scale:0.2})}") %>%
  visEvents(type = "on", selectNode = "function() {
            this.moveTo({
            scale:0.35})}") %>%
  visEvents(type = "on", deselectNode = "function() {
            this.moveTo({scale:0.2})}") %>%
  visNodes(fixed = list("Psalms" = F))

## Export plot ----
visSave(graph = abide, file = "../plots/3.6_connections.html",
        selfcontained = TRUE)

