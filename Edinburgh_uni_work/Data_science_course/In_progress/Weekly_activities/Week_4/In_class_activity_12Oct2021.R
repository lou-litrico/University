# In class activity for Data Science in EES
# Starter script written by Isla Myers-Smith
# 10th October 2019/12th October 2021

# Instructions ----

# Each group will tackle one question. To answer your question you need to use a pipe and dplyr functions. You can also use other functions. Here is a list of the dplyr functions:

# https://dplyr.tidyverse.org/reference/

# Before you start coding, you need to draw a work flow diagram in a file, on paper or a white board for how you are going to design your code. Then you need to take a screen cap/photo of that diagram and upload it to the folder for this activity.

# Then you need to translate your workflow diagram into comments in the R code.

# Then you can start coding.

# Remember that you are working on this script collaboratively so commit, pull and push frequently!


# Starter code ----

# Librarys
library(tidyverse)

# Load Living Planet Data
load("LPI_species.Rdata")

# How many unique species are there?
# Step 1: Find the name of the "species" column
# Step 2: Check that the species and genus columns
# are character columns (e.g., not numbers)
# Step 3: Use the unique function or the distinct function
# Step 4: Use length() to get the length of the list of unique species

# Step 1:
head(LPI_species)

# Step 2:
str(LPI_species)

# Step 3 and 4:
LPI_species %>%
  summarise(n = length(unique(Species)))


# Group 1 ----
# How many bird species are monitored, what are the three most monitored birds?  
#How many mammal species are monitored, what are the three most monitored mammals?  Are birds monitored on average for longer than mammals across all populations?



# Group 2 ----
# What are the top three countries with the most populations monitored?  What are the bottom three countries with the least populations monitored?  What country has on average the longest duration of population monitoring?



# Group 3 ----
# Rank all of the biomes from most to least well represented with populations monitored?  What is the biome with the most different genera monitored?



# Group 4 ----

# What are the top three the most frequent monitoring methods in the monitoring of populations? 
# What is the most common monitoring method for populations that have been monitored from 1970 to 2000?

# Question 1 
sampletype2 <- LPI_species %>% 
  group_by(Sampling.method) %>% 
  tally()  # same way of doing the next part of code 

sampletype <- LPI_species %>% 
  group_by(Sampling.method) %>% 
  summarise(n = length(pop)) %>%  # counts
  arrange(-(n)) %>%  # arrande in descending order (can also do arrange(desc(n)))
  ungroup()
print(sampletype[1:3, ])  # show us the top 3 

# Question 2
str(LPI_monitoring_1970_2000)

# need to get year as a number (factor rn)
LPI_monitoring_1970_2000 <- LPI_species %>% 
  mutate(year = str_remove_all(year,"X"),  # changes year into a character
         year = as.numeric(year)) %>%   # changes it into a number string 
# mutate(year = parse_number(as.character(year)))
  filter(year %in% c(1970:2000)) %>%  # can also do (year >= 1970 & year <= 2000) 
  group_by(Sampling.method) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ungroup()

print(LPI_monitoring_1970_2000[1, ])

# Group 5 ----
# How many different authorities are there for the species in the LPI database? Are there any formatting differences in the Authority list? How many LPI species have been first named by Linnaeus?  Of the taxa named by Linneaus, what class is most common and least common in the database?
