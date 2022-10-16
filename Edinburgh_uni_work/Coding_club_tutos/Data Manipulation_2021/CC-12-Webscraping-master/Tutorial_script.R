## CC TUTORIAL WEB SCRAPING ## ----

# Set wd + Load data and packages ----
setwd("~/Desktop/Autres/Github/louise-litrico/Edinburgh_uni_work/Coding_club_tutos/Data Manipulation_2021/CC-12-Webscraping-master")
library(rvest)
library(dplyr)
Penguin_html <- readLines("Aptenodytes forsteri (Emperor Penguin).html")

# Find the species name in the web page ----
grep("Scientific Name:", Penguin_html)
# this tells us that the scientific name appears once in the vector, on line 132
Penguin_html[131:135]  # we can then find it by checking the text around line 132
# so the species name is on line 133
Penguin_html[133]  # check 

# Isolate the line in a new vector 
species_line <- Penguin_html[133]
# Select the species name in the line 
species_name <- species_line %>%
  gsub("<td class=\"sciName\"><span class=\"notranslate\"><span class=\"sciname\">", "", .) %>%  # Remove beginning
  gsub("</span></span></td>", "", .) %>%  # Remove end
  gsub("^\\s+|\\s+$", "", .)  # Remove whitespace and replace with nothing
# way to use gsub() = gsub("Pattern to replace", "The replacement pattern", .) # 
# + see https://ourcodingclub.github.io/tutorials/webscraping/ for explanation 

# Do the same for the common name ---- 
grep("Common Name", Penguin_html)
Penguin_html[130:160]
Penguin_html[150:160]
Penguin_html[151]
common_line <- Penguin_html[151]
common_name <- common_line %>%
  gsub("<td>","",.) %>% 
  gsub("</td>","",.) %>% 
  gsub("^\\s+|\\s+$", "", .)

# Do the same for IUCN category ----
grep("Red List Category", Penguin_html)
Penguin_html[179:185]
Penguin_html[182]
red_cat <- gsub("^\\s+|\\s+$", "", Penguin_html[182])

# Do the same for date of assessment ----
grep("Date Assessed:", Penguin_html)
Penguin_html[192:197]
Penguin_html[196]
date_assess <- Penguin_html[196] %>%
  gsub("<td>", "",.) %>%
  gsub("</td>", "",.) %>%
  gsub("\\s", "",.)

# Create a df with this data ----
iucn <- data.frame(species_name, common_name, red_cat, date_assess)

# Doing the same for several pages at once ----
search_html <- readLines("Search results.html")
line_list <- grep("<a href=\"/details", search_html)
