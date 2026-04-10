#30/3/2026
#Question 1: Do the positive effects of conservation thinning on oak regeneration persist 22 years after thinning?

library(readr)
library(dplyr)

regeneration <- read_csv("processed_data/regeneration_data.csv")

#Filter to exclude area_type "extended" to remove duplicate seedling rows
regeneration_core <- regeneration %>% 
  filter(area_type != "extended" | is.na(area_type))


#Create oak count datasets, both including and excluding trunk shoots
oaks_all <- regeneration_core %>% 
  filter(species == "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density), .groups = "drop")

oaks_noshoots <- regeneration_core %>% 
  filter(species == "Quercus sp.",
         shoot == FALSE) %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density), .groups = "drop")

