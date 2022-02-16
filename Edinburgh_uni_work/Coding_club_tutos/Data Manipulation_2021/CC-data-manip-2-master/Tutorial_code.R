# Coding club Data manipulation 2 #

# Setting wd + library + files----
setwd("~/Desktop/School related/Data Science/course-repository-louise-litrico/Coding_club_tutos/Data Manipulation/CC-data-manip-2-master")

library(dplyr)
library(ggplot2)
library(tidyr)
trees <- read.csv("trees.csv", header = TRUE)

# Count nb of trees per species ----

trees.grouped <- group_by(trees, CommonName)
trees.summary <- summarise(trees.grouped, count = length(CommonName))
trees.summary <- tally(trees.grouped)  # same thing with tally() from dplyr

# same thing with with a pipe 
trees.summary <- trees %>%
  group_by(CommonName) %>% 
  tally()

# using trees data, select 3 species, group them by name and age and count how many there are in those 2 categories 
# should give back a df with 2 variables (CommonName and AgeGroup) and the number of each? == not exactly but almost
# creates a new object with 3 columns = commonname, agegroup and the tally amount for each combination of both those variables 
trees.subset <- trees %>% 
  filter(CommonName %in% c("Common Ash", "Rowan", "Scots Pine")) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()

# Create summary dataframe ----

summ.all <- summarise_all(trees, mean)

# Changing factor levels or categorical data = re-classifying factor ----

# Adding Genus column using case_when()
unique(trees$LatinName)

trees.genus <- trees %>% 
  mutate(Genus = case_when(
    grepl("Acer", LatinName) ~ "Acer", 
    grepl("Fraxinus", LatinName) ~ "Fraxinus", 
    grepl("Sorbus", LatinName) ~ "Sorbus", 
    grepl("Betula", LatinName) ~ "Betula", 
    grepl("Populus", LatinName) ~ "Populus", 
    grepl("Laburnum", LatinName) ~ "Laburnum", 
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus", 
    grepl("Prunus", LatinName) ~ "Prunus", 
    grepl("Pinus", LatinName) ~ "Pinus", 
    grepl("Sambucus", LatinName) ~ "Sambucus", 
    grepl("Crataegus", LatinName) ~ "Crataegus", 
    grepl("Ilex", LatinName) ~ "Ilex", 
    grepl("Quercus", LatinName) ~ "Quercus", 
    grepl("Larix", LatinName) ~ "Larix", 
    grepl("Salix", LatinName) ~ "Salix", 
    grepl("Alnus", LatinName) ~ "Alnus"))
  
# Doing the same with seperate()
# this creates 2 new columns then removes the species one 
trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>% 
  dplyr::select(-Species)

# Changing height levels from 5 to 3 
trees.genus <- trees.genus %>%   # overwriting the data frame 
  mutate(Height.cat =   # creating the new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short", 
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium", 
                     Height == "20 to 25 meters" ~ "Tall"))

# Reordering factor levels ----

levels(trees.genus$Height.cat)  # this doesn't work... WHY?
unique(trees.genus$Height.cat)  # this does... WHY?

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),
                                 labels = c('SHORT', 'MEDIUM', 'TALL'))   # order should match the levels above!

levels(trees.genus$Height.cat)  # now it works... WHY?

# Plotting ----

# create subset with 5 genuses 
trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# 1 map with all Genuses
(map.all <- ggplot(trees.five) +
            geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
            theme_bw() +
            theme(panel.grid = element_blank(),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12)))

# 5 maps for 5 genuses 
trees.plots <- trees.five %>%  # data used
  group_by(Genus) %>%          # group them by the 5 genuses
  do(plots =                   # to use the ggplot gunction within a pipe? 
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(), 
             axis.text = element_text(size = 14), 
             legend.text = element_text(size = 12), 
             plot.title = element_text(hjust = 0.5), 
             legend.position = "bottom"))

# see the graphs 
trees.plots$plots

# save the graphs in wd with another pipe
# do we use a pipe here to save the plots within themselves in a way???
trees.plots %>% 
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

# why not just do this =
ggsave(trees.plots$plots, filename = paste(getwd(), "/", "map-", trees.plots$Genus, '.png', sep = " "), devide = "png", height = 12, width = 16, units = "cm")

# Challenge ----

# finding middle value 
middle_value_northing <- (max(trees.genus$Northing) - min(trees.genus$Northing))/2  +min(trees.genus$Northing)
middle_value_easting <- (max(trees.genus$Easting) - min(trees.genus$Easting))/2 + min(trees.genus$Easting)

# creating new df with column with the quadrants 
trees.quadrant <- trees.genus %>% 
  mutate(Quadrant = 
           case_when(Northing >= c(middle_value_northing) & Easting <= c(middle_value_easting) ~ "NW",
                     Northing < c(middle_value_northing) & Easting <= c(middle_value_easting) ~ "SW",
                     Northing >= c(middle_value_northing) & Easting > c(middle_value_easting) ~ "NE", 
                     Northing < c(middle_value_northing) & Easting > c(middle_value_easting) ~"SE"))
# Calculating species richness 
trees.quadrant.richness <- trees.quadrant %>% 
  group_by(Quadrant) %>% 
  tally()
                          
sp.richness <- trees.quadrant %>%
  group_by(Quadrant) %>%
  summarise(richness = length(unique(LatinName)))
# why do I get different results? 

# Calculating Acer % in each quadrant
trees.quadrant.acer <- trees.quadrant %>% 
  group_by(Quadrant, Genus) %>% 
  tally() %>% 
  group_by(Quadrant) %>% 
  mutate(total = sum(n)) %>% 
  filter(Genus == "Acer") %>% 
  mutate(percent = n/total)


# Making Acer age groups
trees.age <- trees.quadrant %>% 
  filter(Genus == "Acer") %>% 
  group_by(Quadrant, AgeGroup) %>% 
  tally() %>% 
  rename(Count = n) %>% 
  mutate(Age = 
           case_when(AgeGroup %in% c("Juvenile", "Semi-mature") ~ "Young",
                     AgeGroup == "Middle Aged" ~ "Middle Aged",
                     AgeGroup == "Mature" ~ "Mature"))

unique(trees.age$Age)  # to check order
trees.age$Age <- factor(trees.age$Age, 
                         levels = c('Young', 'Middle Aged', 'Mature'))  # to change order
levels(trees.age$Age)  # to check the chenge worked well

# Making Acer counts barplot
barplot.acer <- trees.age %>% 
    group_by(Quadrant) %>% 
    do(plots = 
         ggplot(data = .) +           
         geom_col(aes(Age, Count), fill = "#a9c3e1", colour = "black") +
         theme_bw() +
         labs(title = paste("Barplot of Acer counts at Craigmillar Castle (", .$Quadrant, ")", sep = " ")) +
         xlab("Age Group") +
         ylab("Acer counts"))

barplot.acer$plots  # to see the plots 

barplot.acer %>% 
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "Acer-counts-", .$Quadrant, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

