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
  ba.mean.year(x$diam, x$year)
})

# other example with height
bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family) # create empty list
lapply(bicuar_height_list, mean, na.rm = TRUE)
sapply(bicuar_height_list, mean, na.rm = TRUE) # for more readable output


