# CC tutorial Functions and loops ----
# https://ourcodingclub.github.io/tutorials/funandloops/

# Set WD and load and check data ----
setwd("~/Desktop/Autres/Github/louise-litrico/Edinburgh_uni_work/Coding_club_tutos/Rbasics_2021/CC-5-fun-and-loop-master")
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")
head(trees_bicuar)
str(trees_mlunguya)

# Create a function ---- 
example.fn <- function(x, y){
  x + y
}
example.fn(x = 1, y = 2)

# Function to calculate the base with x 
basal.area <- function(x){
  (pi*(x)^2)/40000
}
basal.area(x = trees_bicuar$diam)

# Function to calculate the base with dbh 
basal.area <- function(dbh){
  (pi*(dbh)^2)/40000
}
trees_bicuar$ba <- basal.area(dbh = trees_bicuar$diam)

# Function to calculate the base with several vectors (like multiple sites 
basal.area <- function(...){
  (pi*c(...)^2)/40000
}
basal.area(trees_bicuar$diam, trees_mlunguya$diam)

# Using a function in a for() loop ----
for(i in list){
   # actions to perform in here
}  # Basic syntax

# Making a list of df to use in the loop 
trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)

# List items can be accessed with double brackets 
# trees[[1]] for eg would call the 1st item of the list = the trees_bicuar df
for(i in 1:length(trees)) {  # means the loop will go through each item of the list one by one and run the function
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)  # the function creates a new "ba" column in each df and applies the function with the diam data from each
}

# Getting the mean basal area for each year ----
# First create a list with df separated per year 
trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)

# Add the basal area calculation to another list with a loop 
mean_ba_list <- list()  # create an empty list to fill
for(i in 1:length(trees_mlunguya_list)) {
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}

# Other option : make all the intermediate calculations in a function ----
ba.mean.year <- function(dbh,year){
  data.frame(
  mean_ba = mean(basal.area(dbh)),
  year = mean(year)
  )
}

# Ty it to see if it works
ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)

# And then add it in the loop 
for(i in 1:length(trees_mlunguya_list)){
  mean_ba_list[[i]] <- ba.mean.year(trees_mlunguya_list[[i]]$diam, trees_mlunguya_list[[i]]$year)
}

# Create a lapply() function ----
# recreate the same action as before 
lapply(trees_mlunguya_list, function(x){
  ba.mean.year(dbh = x$diam, year = x$year)
})

# other example with height
bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family) # create empty list
lapply(bicuar_height_list, mean, na.rm = TRUE)
sapply(bicuar_height_list, mean, na.rm = TRUE) # for more readable output but transforms into vector so can't do more complex stuff

# Creating a function with ifelse() ----
# this function accounts (corrects) the method measurement error for tree height
# and then calculates the Lorey height of the trees with the stem base  nd the adjusted height
stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  lorey_height <- sum(height_adj * ba, na.rm = TRUE)/sum(ba, na.rm = TRUE)
  return = lorey_height
}

trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode) # create list to try it on
lapply(trees_bicuar_list, function(x){
  stick.adj.lorey(height = x$height, method = x$height_method, ba = x$ba)
})

# Using ifelse() with a TRUE/FALSE ----
diam.sum <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE, mean(dbh), NA)
  median_dbh <- ifelse(median == TRUE, median(dbh), NA)
  mean_ba <- ifelse(ba == TRUE, mean(basal.area(dbh)), NA)
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}
diam.sum(dbh = trees_bicuar$diam, mean = TRUE, median = TRUE)

# Creating graphs with a loop ----
library(dlpyr)
library(ggplot2)
LPI <- read.csv("LPI_data_loops.csv")
vulture <- LPI %>% 
  filter(Common.Name == "Griffon vulture / Eurasian griffon") %>% 
  filter(Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vulture, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))

# Creating out own plot theme with a function ----
theme.my.own <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

# same plot with the function theme
(vulture_scatter <- ggplot(vulture, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme.my.own() +                                                    # Adding our new theme!
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))

LPI.UK <- filter(LPI, Country.list == "United Kingdom")
house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
reed.bunting <- filter(LPI.UK, Common.Name == "Reed bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")
Sp_list <- list(house.sparrow, great.tit, corn.bunting, meadow.pipit) # creating a list with the subsets 

# Using a loop to plot all them at once
for(i in 1:length(Sp_list)){
  data <- as.data.frame(Sp_list[i])  # Pourquoi on met pas deux crochets ??? 
  sp.name <- unique(data$Common.Name)
  plot <- plot <- ggplot(data, aes (x = year, y = abundance)) +   
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  ggsave(plot, file = paste(sp.name, ".pdf", sep = ''), scale = 2)
  print(plot)
}
