## CC TUTORIAL WEB SCRAPING ## ----

# Set wd + Load data and packages ----
setwd("~/Desktop/Autres/Github/louise-litrico/Edinburgh_uni_work/Coding_club_tutos/Data Manipulation_2021/CC-12-Webscraping-master")
library(rvest)
library(dplyr)
Penguin_html <- readLines("Aptenodytes_forsteri_(Emperor_Penguin)_Mine.html")
