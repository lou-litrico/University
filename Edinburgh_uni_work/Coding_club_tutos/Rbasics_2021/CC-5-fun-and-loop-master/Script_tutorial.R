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

# Function to calculate the base with x #
basal.area <- function(x){
  (pi*(x)^2)/40000
}
basal.area(x = trees_bicuar$diam)

# Function to calculate the base with dbh #
basal.area <- function(dbh){
  (pi*(dbh)^2)/40000
}
trees_bicuar$ba <- basal.area(dbh = trees_bicuar$diam)

# Function to calculate the base with several vectors (like multiple sites #
basal.area <- function(...){
  (pi*c(...)^2)/40000
}
basal.area(trees_bicuar$diam, trees_mlunguya$diam)

# Using a function in a for() loop ----
for(i in list){
  
}  # Basic syntax

# Making a list of df to use in the loop 
trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)

# List items can be accessed with double brackets 
# trees[[1]] for eg would call the 1st item of the list = the trees_bicuar df
for (i in 1:length(trees)) {  # means the loop will go through each item of the list one by one and run the function
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)  # the function creates a new "ba" column in each df and applies the function with the diam data from each
}
