# Modelling 1 CC tutorial #

setwd("~/Coding_club_tutos/Modelling/CC-8-Modelling-master")

# Libraries ----

# install.packages("agridat")
library(agridat)
library(ggplot2)
library(dplyr)

# Load and check data ----

# Agridat packages has data sets about agriculture 
apples <- agridat::archbold.apple
head(apples)
summary(apples)

# Visualize the data ----

# Define a function first to use later one
theme.clean <- function(){
  theme_bw() +
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
        legend.position = "right")
}

# Make boxplot for effect of spacing on apple yield 
apples$spacing2 <- as.factor(apples$spacing)

(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "\nSpacing (m)", y = "Yield (kg)\n"))

# Run model to test spacing effect on yield ----

apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)
# Call = reminder of the model
# Coefficients = estimates and errors for each factor level
# Intercept = mean for 1st factor level (in this case spacing26)
# Ajusted R-squared = variance explained by the variables we are looking at (about 15% in this case)
# these variance values go from 0 to 1 (model variables explain 100% of the variation in the examined variable)

# Run other model with sheep ----

sheep <- agridat::ilri.sheep
sheep <- filter(sheep, ewegen == "R")  # take away confounding variables 
head(sheep)
sheep.m1 <- lm(weanwt ~ weanage, data = sheep)
summary(sheep.m1)

# we find that age at weaning explains 20% of lamb weight (= sevrage)
# highly significant models 

sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)

# the intercept is weight at age 0 for female = reference group
# weanage = effect of age
# sexM = difference in intercept value for the male reference group
# last term is the interaction = difference in slope for makes 

# Plot the relationship ----

(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
   geom_point(aes(colour = sex)) +                                # scatter plot, colored by sex
   labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
   stat_smooth(method = "lm", aes(fill = sex, colour = sex)) +    # adding regression lines for each sex
   scale_colour_manual(values = c("#FFC125", "#36648B")) +
   scale_fill_manual(values = c("#FFC125", "#36648B")) +
   theme.clean() )

# linear regression, linear model and ANOVA = SAME THING !!!
# only difference is type of data used = first 2 deal with continuous data only 
# ANOVA looks at effect of discrete variable on a continuous variable 

# Checking assumptions ----

# residuals normally distributed?
apples.resid <- resid(apples.m)
shapiro.test(apples.resid)
# good if there is no significant different from a normal distribution

# check for homoscedasticity = variance in the data similar for all values of the predictor variable 
bartlett.test(apples$yield, apples$spacing2)
bartlett.test(yield~ spacing2, data = apples) # same thing different code 
# same as before, all good if no significant difference of variances 

# examine fit further 
plot(apples.m)

# Model with Poisson distribution ----

shag <- read.csv('shagLPI.csv', header = TRUE)
shag$year <- as.numeric(shag$year)

# plot data to find distribution
(shag.hist <- ggplot(shag, aes(pop)) +  # pop = count abundance data so poisson distribution is the one to use
    geom_histogram() +
    theme.clean())

shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)
# tells us that abundance varies significantly every year 

# plot distribution 
(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme.clean() +
    labs(x = " ", y = "European Shag abundance"))

# Model with binomial distribution ----

Weevil_damage <- read.csv("Weevil_damage.csv")
Weevil_damage$block <- as.factor(Weevil_damage$block)

weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)
# We find that the probability of pine tree enduring damage from weevils varies significantly based on the block 

# Challenge ----

ToothGrowth <- datasets::ToothGrowth
# tooth growth in guinea pigs under different vitamin C treatments 
# OJ = Orange Juice 
# VC = Ascorbic acid 

ToothGrowth$dose <- as.factor(ToothGrowth$dose)  # transform into categorical variable 

# Is higher dose better for teeth growth?
# Does source of vitamin influence the effect?
dose.m <- lm(len ~ dose*supp, data = ToothGrowth)  # because both variables continuous?
summary(dose.m)
# So dose amount and source explains 77% of variation in tooth length + significant effect

# didn't understand how to do the last question....
