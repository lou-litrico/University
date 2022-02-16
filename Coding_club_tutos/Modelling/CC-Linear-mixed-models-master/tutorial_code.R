# Linear modelling tutorial # 

# Libraries ----

library(ggplot2)
library(lme4)
library(ggeffects)
library(tidyverse)
library(sjPlot)
library(stargazer)

# Load and check data ----

load("dragons.RData")
head(dragons)
# testScore = intelligence 
hist(dragons$testScore)  # looks close to a normal distribution so good

# standardize the explanatory variable to have a mean of 0 (center) and a SD of 1 (scale)
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

# Linear analysis for all data ----
# ignore mountain range and site for now
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

# plot data
(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

# check assumptions 
plot(basic.lm, which = 1)  # not perfect
plot(basic.lm, which = 2)  # a bit off at the extremes, but doesn't look too bad

# look at effect of mountain range 
boxplot(testScore ~ mountainRange, data = dragons)  # looks like something is going on here

(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

# shows that mountain range affects both intelligence and body length of dragons 
# + that observations within each range are NOT independent 
# need to do one analysis per mountain range + other ones for site differences 
# BUT the more specific we get, the more we reduce our sample size 

# linear model with a fixed effect ----

# without looking at site differences for now, we get...
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)
# body length is not significant here and this model shows the effect of each mountain range
# even though what we want is the effect of body lengths taking into account the effect of mountain range differences 

# Mixed effects models ----

# difference between fixed and random effects 
# fixed effects = explanatory variable, you expect it will have an effect
# random effects are grouping factors for which we are trying to control 
# + are always categorical 
# model tells us how much variation in the data is due to this grouping variable 
# random effect should usually have 5 or more levels = sex would be a fixed effect not a random one

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)  # 1|VariableName looks at the random effect
summary(mixed.lmer)
339.7/(339.7 + 223.8)  # shows us that mountain range account for ~60 % of variance that's not explained by the fixed variable 

# plot model results 
plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line = good!

# difference between crossed and nested random effects 
# nested random effects = show a hierachical data structure = stratification OR different observations in the levels of the effect
# crossed random effects = no hierarchy OR when same observations in all the levels of the effect 

dragons <- within(dragons, sample <- factor(mountainRange:site))  # creatin a variable that takes nestling into account
# wrong model doesn't take nesting into account 
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)  # wrong bc finds 3 sites when we actally sampled 24 areas

# better model
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)  # accurately looks at 24 sites spread between 8 mountain range 

# plot results
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Random slopes ----
# previous model allows only for random intercept and not for random slope 
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)

# plot results 
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# plotting model predictions ----

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

# if you want to visualise how the relationships vary according to different levels of random effects? 
# You can specify type = "re" (for “random effects”) in the ggpredict() function
# and add the random effect name to the terms argument.

# other method to plot without ggplot
ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

# if you are interested in showing the variation among levels of your random effects
# is to plot the departure from the overall model estimate for intercepts 
# and slopes, if you have a random slope model

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))
# 1 is for each site within each mountain range and 2 is just for the 8 mountain ranges
# show summary
summary(mixed.ranslope)
# The values you see are NOT actual values but rather the difference between the general intercept 
# or slope value found in your model summary and the estimate for this specific level of random effect.

# using a table to show model outputs 
stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
