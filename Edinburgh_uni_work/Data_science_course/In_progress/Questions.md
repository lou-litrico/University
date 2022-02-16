# Questions from the course or the coding club tutorials 

## Data manipulation tutorials 
- Why doesn't the levels() function work all the time? and how is it different from the unique() function
```
levels(trees.genus$Height.cat)  # this doesn't work... WHY?
unique(trees.genus$Height.cat)  # this does... WHY?
```
- Once I do the next step (changing the order of the factor levels) it works again... why? 
```
trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),
                                 labels = c('SHORT', 'MEDIUM', 'TALL'))   # order should match the levels above!

levels(trees.genus$Height.cat)  # now it works... WHY?
```

- What is the do() function used for? is it to used ggplot in a pipe? 
```
trees.plots <- trees.five %>%
  group_by(Genus) %>%          # group them by the 5 genuses
  do(plots =                   # to use the ggplot gunction within a pipe? 
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(), 
             axis.text = element_text(size = 14), 
             legend.text = element_text(size = 12), 
             plot.title = element_text(hjust = 0.5), 
             legend.position = "bottom"))
```
- And in this other example, are we saving the plots within their object? using a pipe? and the do() function???
```
trees.plots %>% 
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))
```
- When calculating species richness in the second tutorial, why do these two chunks of code give me different results? 
```
trees.quadrant.richness <- trees.quadrant %>% 
  group_by(Quadrant) %>% 
  tally()
```
```
sp.richness <- trees.quadrant %>%
  group_by(Quadrant) %>%
  summarise(richness = length(unique(LatinName)))
```
- Why don't we need quotation marks around the year in this piece of code? 
```
elong_total <- mutate(elongation, total.growth = X2007 + X2009 + X2010 + X2011 + X2012)
```
- When combining two datasets by columns like here, how does R choose which column name to keep? 
```
experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))
```

## Modelling tutorials 
- Is it possibel to have an ajusted R-squared value of 0? if yes what does that imply for our model (that we didn't look at the right variables?) 
- And can we have a significant p-value but not due to the variables we are looking at? 
- Is the predictor variable the same as the explanatory variable?
- In the second tutorial, part 8, why do we put 1 and not I before the year and site variables? 
```
# effect of mean T° on richness
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)
```
- For the MCMCglmm models, when the tuto says the effective sample size has to be bigger than 1000, are we talking only about the fixed effect one or all of them? And why is the fixed effect one so much lower for T° (169) than for time (1223)?
- When looking at the histogram of variance for random effects, how do you present that result (pushed against 0 or not) to show that random effects were not significant in a paper? 
- We check for convergence by looking at the trace plot, but what does the density plot tell us? 
- 
