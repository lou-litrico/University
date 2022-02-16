# Data visualization 3 

setwd("~/Coding_club_tutos/Data visualisation/CC-dataviz-beautification-master")

# Libraries ----
library(tidyverse)
library(ggthemes)  # for a mapping theme
library(ggalt)     # for custom map projections
library(ggrepel)   # for annotations
library(viridis)   # palette 

# Load data ----
lter <- read.csv("lter.csv")
niwot_plant_exp <- read.csv("niwot_plant_exp.csv")

north_america <- map_data("world", region = c("USA", "Canada"))
north_america <- north_america[!(north_america$subregion %in% "Hawaii"),]
