Forest Cover Change in Parque do Xingu, Brazil (2000-2016)

Group 2: Emily Tanner, Louise Litrico, Matus Seci, Nadia Sotgiu, Helene Engler, Anna Cumming

Date: November 25, 2021

## Research question
__ How has forest cover changed within Parque do Xingu, Brazil from 2000-2016)?__

## Introduction
Parque do Xingu in Mato Grosso, Brazil was designated as a national park in 1961 to protect the territory of the indigenous Xingu people and preserve its natural habitat. The old-growth forests in this area are some of few that have persisted through the deforestation arc in Brazil that follows its political interests. In spite of this protected area status a combination of intensive cattle ranching, forestry operations and, the prevelant issue of late, the intrusion of fires from slash and burn agriculture has resulted in the loss of a vast area of forest. But the Xingu peoples have been working with the Amazon Conservation Team (ACT) to reduce illegal intrusions and promote the conservation of this habitat and its species. Even with the addition of new trees, the habitat will not have the same biodiversity as the old-growth forest but without adding new trees it seems likely that this area will have a substantial loss of forest cover. With the political changes over the year it is likely that the level of deforestation will vary too. We therefore sought to answer the question: How has forest cover changed within Parque do Xingu, Brazil from 2000-2016?

## Workflow

* Matus = research of new GEE dataset to explain forest cover change <br />
* Helene = data visualization = overall forest area change over the years 
* Louise = data manipulation to turn dataset into long format 
* Anna = markdown basic information 
* Nadia = theme creation
* Emily= help with markdown
* organise repo
* get picture of the map (Matus)
* finish figure (Helene)
* finish markdown with predictions (anna and emily)

## Specific hypotheses and predictions

Given the prevalence of slash and burn management techniques in Brazil, we predict we will observe more forest loss than gain. We believe we will see a loss in early 2000s and a gain in forest cover due to shifts in Brazilian policy (Harari, 2019). However, we do expect to see annual variation given fire cycles and seasonal vegetation changes in our plots (Campbell, 1986).

# Methods

### Datasets
We used the Hansen et al. Global Forest Change dataset v1.5 (Hansen et al., 2013) to obtain raster data showing forest gain and loss between years 2001 - 2016.

We used the data from World Database of Protected Areas (WDPA, 2021) to obtain a vector of the Parque do Xingu protected area to be used as a template for extracting information on gain/loss from the dataset above.

### Analysis
We extract forest gain and loss information between years 2001 - 2016 using year 2000 as a base year against which loss/gain values were calculated in the Parque do Xingu using Google Earth Engine. 

The data were then downloaded and further analysed in R. This analysis involved calucalting the forest cover change as change = loss - gain and visualzing the change as a barplot. 


## Data vis and summary methods


```r
forest_change <- DoXingu_forest %>% 
  pivot_longer(cols = 30:62, names_to = "measure", values_to = "forest_cover") %>%
  pivot_longer(cols = 30:62, names_to = "measure", values_to = "forest_cover") %>% 
  mutate(year = parse_number(measure)) %>% 
  na.omit() %>% 
  mutate(category = case_when(grepl("loss", measure) ~ "loss"),
                              grepl("gain", measure) ~ "gain")

# create vne ariables for forest gain and loss 
gain <- forest_change %>% 
  filter(.$change == "gain")

loss <- forest_change %>% 
  filter(.$change == "loss")
  
  #create final loss variable (=overall forest loss) 
change <- left_join(gain, loss, by = "year") %>% 
  select(year, change.x, change.y, forest_cover.x, forest_cover.y) %>% 
  mutate(net_change = forest_cover.y-forest_cover.x)
```

# 1. Maps of forest cover change for your protected area

![Figure 1](https://github.com/EdDataScienceEES/challenge4-greoup2/blob/master/pictures/map_area.png)
*Figure 1: Map of forest cover change in Parque do Xingu in Brazil (green area represent forest cover, orange represents forest gain and purple represents forest loss)*

# 2. Visualisation of the amount of forest cover loss and gain for your protected area


![Figure 2](https://github.com/EdDataScienceEES/challenge4-greoup2/blob/master/pictures/forest_change_barplot.jpeg)

*Figure 2: Barplot of forest cover loss in Parque do Xingu between 2000 and 2017 and trend*


# 3. How do your results compare with your predictions? What do you think might explain the patterns you found?

Our results confirmed our predictions that we would observe a substantial loss in forest cover. We also predicted there would be considerable variation due to seasonal patterns, fire cycles, and policy changes. Our results confirmed this hypothesis, this is visible Figure 1 and 2 above.


# 4. What other datasets, available within the GEE, could you use to test the potential drivers of forest cover change in your group's protected area that you identified in point #3. ?

- Copernicus Global Land Cover Layers: CGLS-LC100 Collection 3 (https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_Landcover_100m_Proba-V-C3_Global#description)

Land Cover Layer provides data about global land cover change in years 2015-2019 and includes information on land cover classes such as forest, grassland and crops. This dataset would therefore allow us to determine into what land cover types the forest was converted, for example if we see an increase in the crops-coverfraction variable in a particular pixel we could assume conversion to cropland such as soy, maize or sugar cane which are common cash crops grown in the Brazilian Amazon or if we see an increase in grass-coverfraction we could assume transition to pasture used for cattle ranching (another common type of deforestation driver in the Brazilian Amazon).

- MapBiomas Project (https://mapbiomas.org/en/products)
MapBiomas is a project for monitoring change in the Brazilian Ecosystems set up by organizations in Brazil. It contains data from 1985-2020 on land cover and use which can be mapped against variables such as fire burns, sizes of properties, mining and many more. It allows conducting a detailed analysis and exploring of drivers of land cover change. 


# 5. What research question and hypotheses would you test with those additional datasets in your proposed future research and why does that research matter for the park management?

Given more time and resources, we might be able to further examine issues including looking into data quality, and perhaps expanding the temporal data structure to ensure that we are observing a short-term trend. We might also consider how we could incorporate any data or documentation of illegal cutting and burning of forest areas. With more resources, we might be able to access data sets made available to understand complexity of inputs in the Brazilian ecosystem of Xingu Indigenous Park.

## Conclusions

Overall we have found a considerable decrease in forest cover from the period of 2000-2016.


# References

* Campbell, D.G., Daly, D.C., Prance, G.T. and Maciel, U.N., 1986. Quantitative ecological inventory of terra firme and várzea tropical forest on the Rio Xingu, Brazilian Amazon. Brittonia, 38(4), pp.369-393.

* Harari, I., 2019. Every minute, 533 trees are cut down in the Xingu basin. [online] ISA - Instituto Socioambiental. Available at: <https://www.socioambiental.org/en/blog/blog-do-xingu/every-minute-533-trees-are-cut-down-in-the-xingu-basin> [Accessed 25 November 2021].

* Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line at: https://earthenginepartners.appspot.com/science-2013-global-forest.

* UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) [On-line], [Nov 2021], Cambridge, UK: UNEP-WCMC and IUCN Available at: www.protectedplanet.net.

* Xingu Indigenous Park, Wikipedia (available online : (https://en.wikipedia.org/wiki/Xingu_Indigenous_Park))

* Places to Watch: 3 Forests Experiencing Rapid Clearing Right Now, available online: (https://www.globalforestwatch.org/blog/places-to-watch/places-to-watch-3-forests-experiencing-rapid-clearing-right-now/)

* Slash-and-burn clearing nears Indigenous park as Brazil’s fire season ignites, Morgan Erickson-Davis on 10 June 2021, available online: (https://news.mongabay.com/2021/06/slash-and-burn-clearing-nears-indigenous-park-as-brazils-fire-season-ignites/)
