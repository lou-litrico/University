# Group 2 challenge 4
# Nadia Sotgiu, Emily Tanner, Helene Engler, Louise Litricio, Anna Cumming, Matus Seci
## Looking at forest cover change in Parque do Xingu from 2001-2016
## 25th November, 2021

# Libraries
library(ggplot2)
library(tidyverse)

# Load data 
DoXingu_forest <- read_csv("data/forest_change_Parque_do_Xingu.csv")

# rename data fram (the long format)
forest_change <- DoXingu_forest %>% 
  pivot_longer(cols = 30:62, names_to = "measure", values_to = "forest_cover") %>%
  pivot_longer(cols = 30:62, names_to = "measure", values_to = "forest_cover") %>% 
  mutate(year = parse_number(measure)) %>% 
  na.omit() %>% 
  mutate(category = case_when(grepl("loss", measure) ~ "loss"),
                              grepl("gain", measure) ~ "gain")

# create vne ariables for forest gain and loss 
gain <- forest_change %>% 
  filter(.$change == "gain")

loss <- forest_change %>% 
  filter(.$change == "loss")

#create final loss variable (=overall forest loss) 
change <- left_join(gain, loss, by = "year") %>% 
  select(year, change.x, change.y, forest_cover.x, forest_cover.y) %>% 
  mutate(net_change = forest_cover.y-forest_cover.x)

str(change)

# create plot (need data)
(ggplot(change, aes(x = year, y = net_change)) +
    geom_bar(stat = "identity", fill="#52854C") +
    geom_line() +
    labs(title= "Forest Cover Change in Parque do Xingu between 2000 - 2017",
         y="Forest cover loss [km"^{2}~"]", x = "Year") + 
    theme(text = element_text(family = "Times New Roman"), 
          axis.text = element_text(size = 12), 
          axis.title = element_text (size = 14), 
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))))

ggsave("pictures/forest_change_barplot.jpeg")
  
