#27/4/2026
#Question 3: How do these factors (basal area, canopy openness, pH) together influence long-term oak regeneration following conservation thinning, compared to the undisturbed control plots?

library(readr)
library(dplyr)

regeneration <- read_csv("processed_data/regeneration_data.csv")

#Create a dataset in which basal area measurements are from the core area
regeneration_core <- regeneration %>% 
  filter(area_type != "extended" | is.na(area_type))

#Create oak count datasets, both including and excluding trunk shoots
oaks_all <- regeneration_core %>% 
  filter(species == "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density), 
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba),
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")

#Note: exclude shoots inside the sum so zero-oak seedling subplots are kept
oaks_noshoots <- regeneration_core %>% 
  filter(species == "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density[shoot == FALSE]),
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba), 
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")

