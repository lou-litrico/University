# Professional Skills Stats Assignment #
# B139919
# started 21/11/21

# Libraries ---
library(tidyverse)
library(ggeffects)
library(sjPlot)

# Load and check data ----
Inga <- read.csv("Inga_traits.csv")
view(Inga)
str(Inga)

# Exercise 1 ---- 
# question a ----
(leaf_area_hist <- ggplot(Inga, aes(x = Leaf_Area)) +
   geom_histogram() +
   theme_classic()) +
   xlab("\nLeaf area (cm"^{2}~")") +
   ylab("Counts\n")

hist(Inga$Leaf_Area)
# in statistical terms, the data doesn't really follow a normal distribution. 
# It doesn't have any negative values and is skewed towards lower values (around 40)

# question b ----
(leaf_area_hist <- ggplot(Inga, aes(x = log(Leaf_Area))) +
    geom_histogram() +
    theme_classic()) +
    xlab("\nLog-transformed leaf area (cm"^{2}~"2)") +
    ylab("Counts\n")

# question c ----
# The data that was collected is the leaf area, the average size of leaves for a specific species in centimeters squared.
# the first distribution histogram shows us that most species have a leaf area value ranging between 10 and 60. 
# A few species then have a leaf area ranging between 60 and 100. And then one single species have a leaf area of about 140. 

# Exercise 2 ----
# question a ----
(habitat_phos_boxplot <- ggplot(Inga, aes(x = Habitat, y = P_Leaf)) +
  geom_boxplot() +
  theme_classic() +
  xlab("\nSpecies habitat") +
  ylab("Leaf phosphorus concentration (mg/g)\n"))

# question a2 ----
habitat_phos_lm <- lm(P_Leaf ~ Habitat, data = Inga)
summary(habitat_phos_lm)
# F statistic = 8.598
# p-value = 0.0013
# DF = 2 and 27 

# linear model means 2 DF for intercept and slope and 1 for the last DF used to calculate the residuals = 30 - 2 - 1 = 27 
# 30 because 7 species have NA values 
# say what we use the degrees of freedom for 
# F Statistic = variance of the group means / mean of the within group variances = so tells us how much variation is due to within or between categories 
# for our DF, F critical value is 5.49, our F statistic is bigger so we can reject the null hypothesis and assume that our effect is significant 
# this is done by the p-value already 
# computed F stat and null F stat = p-value 
# With the DF of our data set and the F stat calculated by the lm, the p-value tells us that this F stat is significantly bigger than the critical F value 
# which separates the null F distribution and our distribution. Thanks to this small p-value, we can infer that the variation between categories (habitats) is bigger than the variation within habitat.
# so habitats in our data have significant differences in Phosphorus content in the leaves 

# question b ----
# Assumption 1 = normally distributed residuals or population
plot(habitat_phos_lm)
# Q-Q plot shows a fairly normal distribution with some outliers (cinnamomea and tomentosa) so not sure it's good
# make a Shapiro-Wilk test to check
resid_habitat_phos <- residuals(habitat_phos_lm)
shapiro.test(resid_habitat_phos)
# p-value is non significant so we accept the null hypo that our residuals aren't normally distributed around the mean variance = so good
# another assumption of a linear model is the equal variances in the population so bartlett test to look at that
bartlett.test(P_Leaf ~ Habitat, data = Inga)
# p-value significantly small so this is not good = we reject the null hypo that the variances are equal = variances not equal = assumtion violated 
# third assumption is that the independence of groups but we cannot know this for sure as we did not collect the data ourselves = can only assume that this assumtion is met

# question c ----
# make it better by using the log-transformed phosphorus concentration 
habitat_phos_lm2 <- lm(log(P_Leaf) ~ Habitat, data = Inga)
summary(habitat_phos_lm2)
# F statistic = 10.12
# p-value = 0.0005
# DF = 2 and 27 
# Assumption 1 = met
plot(habitat_phos_lm2)  # looks better
resid_habitat_phos2 <- residuals(habitat_phos_lm2)
shapiro.test(resid_habitat_phos2)  # better results = higher p-value
# Assumption 2 = met
bartlett.test(log(P_Leaf) ~ Habitat, data = Inga)  # p-value = 0.1 so cannot reject null hypo so variances are equal 
# Assumption 3 = assumed to be met

# question d ----
# floodplain has highest P concentration and upland has the lowest because rainfall and infiltration 
# takes lots of soil nutrients from upland and deposits them into the floodplain 
# This means that the plants that grow in those areas have access to different amounts of soil nutrients 
# Phosphorus can be one of those nutrients that occur in different concentrations in different habitats.
# and when the plants grow and suck up the P from the ground, they end up reflecting the soil concentration in their own leaf concentration
# so this could be why we find significantly different concentrations of phosphorus in plant species found in different habitats

# Exercise 3 ----
# question a ----
(phos_carbon_plot <- ggplot(Inga, aes(x = C_Leaf, y = P_Leaf, shape = Habitat, color = Habitat)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, aes(fill = Habitat)) +
  theme_classic() +
  xlab("\nLeaf Carbon concentration (mg/g)") +
  ylab("Leaf Phosphorus concentration (mg/g)\n"))
# Figure 1: relationship (points and trendline) between leaf carbon and phosphorus concentrations in different habitats. 
# Red points represent floodplain species, green triangles represent generalist species and blue squares represent upland species.

# question b ----
# Generalist and upland species sho similar patterns of a positive relationship between phosphorus and carbon concentrations 
# (increasing carbon concentration leads to increasing phosphorus  concentration).
# In comparison, floodplain species show a negative relationship between those two concentrations.

Inga <- Inga %>% 
  mutate(Habitat_new = case_when(grepl("generalist", Habitat) ~ "upland_generalist",
                                 grepl("upland", Habitat) ~ "upland_generalist",
                                 grepl("floodplain", Habitat) ~ "floodplain"))

new_habitat_lm <- lm(P_Leaf ~ C_Leaf*Habitat_new, data = Inga)
# explain choice of including no interaction = the previous models showed us that C_Leaf is significantly affected by Habitat 
# So variables not independent so need to understand the interactive 
summary(new_habitat_lm)

# make a table to present results maybe
# Add up the estimates that are significant + Intercept is the category that doesn't appear in the other estimates = floodplain
# report the t values (each values compares to the intercept, if one is different from ±2.20) = shows that not really a significant difference
# report p values = intercept significantly different from 0, interaction and habitat alone both significant but only Carbon is not
# Upland and generalist species have significantly lower phosphorus leaf concentration than floodplain species 
# variance explained by habitat = (estimate of habitat= intercept estimate (0.79417) - habitat estimate (1.1397) = -0.34555)
# difference between floodplain and upland_generalist habitat in P concentration is -1.1397
# The interaction between carbon concentration and habitat had marginally significantly negative effect (estimate = -0.33601) 

# question c ----
plot(new_habitat_lm)
# Residuals vs leverage plot shows that species tomentosa could be driving the relationships and effects we get 
# solution might be to run the model again without that outliers to check how different the model outputs are
# Scale-Location checks for equal variance among residuals = homoscedasticity and doesn't look too horrible although could be better
# Normal Q-Q plot checks normal distribution of residuals = looks okay but might be better to remove the outliers (tomentosa and cinnamomea)
# Residuals vs Fitted shows if linear regression is appropriate for our dataset =  if the residuals follow a linear pattern = doesn't look too bad

Inga_no_tomentosa <- Inga %>%
  filter(!Species == "tomentosa", !Species == "cinnamomea")

new_habitat_lm2 <- lm(P_Leaf ~ C_Leaf*Habitat_new, data = Inga_no_tomentosa)
summary(new_habitat_lm2)
# F statistic = 26.14
# DF = 3 and 24
# p-value of intercept = floodplain habitat = 7.27e-07 + t value = 6.637
# p-value of slope = upland_generalist habitat = 2.85e-06 + t value = -6.073
# p-value of slope = C_Leaf = 4.47e-06 + t value = -5.890
# p-value of interaction = 3.71e-06 + t value = 5.966 
# effect of habitat = -0.345545
# effect of carbon = 1.295919
# effect of interaction = -0.33601
plot(new_habitat_lm2)

# question d = report in simple words ----

# Exercise 4 ----
Inga_subset <- Inga %>% 
  select(Trichome_Density, Mevalonic_Acid, Expansion) %>% 
  na.omit()

# question a ----
expansion_gml <- glm(Mevalonic_Acid ~ Expansion, family = binomial, data = Inga_subset)
summary(expansion_gml)
# expansion has p-value of 0.0275 + t value = 2.348 
# estimate of effect of expansion is -0.189869

trichome_glm <- glm(Mevalonic_Acid ~ Trichome_Density, family = binomial, data = Inga_subset)
summary(trichome_glm)
# trichome density change doesn't appear to have any effect on whether or not trees produce mevalonic acid in their leaves 
null_glm <- glm(Mevalonic_Acid ~ 1, family = binomial, data = Inga_subset)

AIC(null_glm, expansion_gml, trichome_glm)
# trichome is less than 2 AIC points lower than the null model so doesn't explain the variation of the data better than the null model
# expansion has a lower AIC value than both other models 
# so expansion model explains more of the variation in the melavonic acid production 

# question b ----
expansion_trichome_glm <- glm(Mevalonic_Acid ~ Trichome_Density + Expansion, family = binomial, data = Inga_subset)
summary(expansion_trichome_glm)
AIC(null_glm, expansion_gml, trichome_glm, expansion_trichome_glm)
# expansion still has an effect (more significant) than before
# trichome density still no effect
# AIC shows that this model explains more of the data variation than the others so good 

# question c = explain ----

# question d ----
Inga_yesorno <- Inga_subset %>% 
  mutate(Mevalonic_Acid = case_when(grepl(1, Mevalonic_Acid) ~ "yes",
                                    grepl(0, Mevalonic_Acid) ~ "no"))

(expansion_plot <- ggplot(Inga_subset, aes(x = Expansion, y = Mevalonic_Acid)) +
  geom_point() +
  theme_classic() + 
  geom_line(data = predictions, aes(x = x, y = predicted), size = 1) +
#  geom_ribbon(data = predictions, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.1) +
  xlab("\nExpansion of leaf size (%/day)") +
  ylab("Production of Mevalonic acid (present/absent (1/0))\n"))

predictions <- ggpredict(expansion_trichome_glm, terms = c("Expansion"))

(expansion_plot3 <- plot_model(expansion_trichome_glm, show.values = TRUE))

(ggplot() +
  geom_ribbon(data = predictions, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.1))
