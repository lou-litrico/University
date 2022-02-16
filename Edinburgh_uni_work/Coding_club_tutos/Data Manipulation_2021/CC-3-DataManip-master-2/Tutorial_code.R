# Coding club data manipulation 1 # 

setwd("~/Desktop/School related/Data Science/course-repository-louise-litrico/Coding_club_tutos/Data Manipulation/CC-3-DataManip-master-2")

# Import and check data ----
elongation <- read.csv("EmpetrumElongation.csv", header = TRUE)  # header = true if the first line contains the titles of each column 

head(elongation)
str(elongation)

elongation$Indiv  # gives all ID codes 
length(unique(elongation$Indiv))  # gives the number of different distinct trees 
elongation[2,5]  # to get value in row 2, column 5 
elongation[6, ]  # to get all values in row 6
elongation[6, ]$Indiv  # to get Indiv value in row 6 
elongation[elongation$Indiv == 603, ]  # to get values (row) of individual nb 603
# this is for numerical values, add "..." around 603 if looking for specific character or factor type data

# OPERATORS FOR LOGICAL OPERATIONS ----

# == for "equals exactly"
# < or <= for "smaller than", or "smaller or equal than" 
elongation[elongation$Zone < 4, ]  # gives values for zones 2-3 only
elongation[elongation$Zone <= 3, ] # different code for the same thing
# > or >= for "bigger than", or "bigger or equal than" 
# != for "not equal to"
# %in% for value belongs to ... (vector of possible values)
# & to chain two conditions that should both be met
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:500), ]  # to get values of Zone 2 with an ID nb between 300 and 500
# | to chain 2 conditions and at least one should be met
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]  # gives values for zones 2 and 7
# ! for things that should be omitted 

# Copy the data and change variables ----
elong2 <- elongation

names(elong2)  # returns the names of columns = the header (?)
names(elong2) [1] <- "zone"  # changes Zones to zones 
names(elong2) [2] <- "ID"

elong2[1,4] <- 5.7  # to hange a specific value
elong2[elong2$ID == 373, ] $X2007 <- 5.7 # same thing but better code since will work even if placement of value changes

# Creating a factor ----
elong2$zone <- as.factor(elong2$zone)  # turns zone column into a factor rather than an intergral
str(elong2)  # check that it worked 

levels(elong2$zone)  # returns the different levels 
unique(elong2$zone)  # what's the difference between the previous?  
levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F")  # changes the name of the levels 

# Tidy data has 1 row for each observation and 1 column for each variable ----

library(tidyr)

# To get the lengths ordered by year ----
elongation_long <- gather(elongation, Year, Length,   # data, key (name), value 
                          c(X2007, X2008, X2009, X2010, X2011, X2012))  # which values to include
elongation_long2 <- gather(elongation, Year, Length, c(3:8))  # does the same 
# creates a new df with 684 observations of 4 variables because 6(years)*114(different lenghts) = 684

elongation_wide <- spread(elongation_long, Year, Length)  # opposite function, goes back to original elongation df

# Plot the growth (Length) by year ----

boxplot(Length ~ Year, data = elongation_long, 
        xlab = "Year", ylab = "Elongation (cm)", 
        main = "Annual growth of Empetrum hermaphroditum")
# Rename variables with dplyr ----
# rename(data, new names = old name)
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length)

# Filter to select observations ----
# to get observations from zone 2 + 3 and years 2009-2011
elongation_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011"))
# same thing in base R
elongation_subset2 <- elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]
# need quotes "" around value if made of text rather than numbers (like year rather than zone)

# Select columns ----

elong_no.zone <- dplyr::select(elongation_long, indiv, year, length)
elong_no.zone <- dplyr::select(elongation_long, -zone)  # same thing
elong_no.zone <- elong_long[ ,-1]  # for same thing in base R

# rename and reorder columns 
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

# Create new columns with mutate() ----

elong_total <- mutate(elongation, total.growth = X2007 + X2009 + X2010 + X2011 + X2012)
# QUESTION = why no need to put the years in quotes? + why does putting them in quote not work?

# Summarise data with group_by() ----

elong_grouped <- group_by(elongation_long, indiv)
# looks the same as elongation_long BUT data has been grouped by individuals 
# always create new object for grouped data since we lose info from the other variables 
summary1 <- summarise(elongation_long, total.growth = sum(length))  # gives a total sum of lengths
summary2 <- summarise(elong_grouped, total.growth = sum(length))  # gives a total length for each individual since they were set in place (grouped)
# QUESTION = why is the total length value different for summary2 and elong_total???

# makes it possible to get interesting info about the data 
summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

# Merge datasets with join() ----

# full_join() keeps everything from both datasets 

treatments <- read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")

experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))
experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  # base R equivalent

# QUESTION = how does R choose what to call the new joined columns? = takes name from 1st or 2nd dataset? 

# Plot effect of treatment on growth ---- 
boxplot(length ~ Treatment, data = experiment, 
        xlab = "Treatment", ylab = "Stem length (cm)", 
        main = "Warmong and fertilisation effects on Empetrum hermaphroditum growth")

# Challenge ----

dragons <- read.csv("dragons.csv")

# Rename paprika to turmeric 
dragons <- rename(dragons, turmeric = paprika)

# Changing tabasco variables 
dragons[dragons$species == "hungarian_horntail", ] $tabasco <- dragons[dragons$species == "hungarian_horntail", ] $tabasco -30
dragons[dragons$species == "hungarian_horntail", ] $tabasco # to check if it worked

# Tranform into long data
dragon_long <- gather(dragons, Spice, Length, c(tabasco, jalapeno, wasabi, turmeric))

# Convert length to meters
dragon_long$Length <- dragon_long$Length / 100

# Create species specific subsets 
levels(dragon_long$species)  # to check the number and name of species 
horntail <- filter(dragon_long, species == "hungarian_horntail")
shortsnout <- filter(dragon_long, species == "swedish_shortsnout")
green <- filter(dragon_long, species == "welsh_green")

# Boxplots
boxplot(Length ~ Spice, data = horntail,
        xlab = "Spices", ylab = "Plume size (m)")
boxplot(Length ~ Spice, data = shortsnout,
        xlab = "Spices", ylab = "Plume size (m)")
boxplot(Length ~ Spice, data = green,
        xlab = "Spices", ylab = "Plume size (m)")

# I DID IT WITHOUT LOOKING AT THE SOLUTION!!!!!!!!!