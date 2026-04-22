#21/4/2026
#Question 2: How do forest structure, light availability, and soil conditions influence oak seedling density across the study sites?

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


#Create a dataset in which basal area measurements are from core+extended
regeneration_extended <- regeneration %>%
  group_by(site, plot, treatment, year, transect, subplot) %>%
  mutate(total_ba = case_when(any(area_type == "extended", na.rm = TRUE) ~ 
                                first(total_ba[area_type == "core"]) + first(total_ba[area_type == "extended"]),
                              any(area_type == "core", na.rm = TRUE) ~ 
                                first(total_ba[area_type == "core"]),
                              TRUE ~ first(total_ba)),
         quercus_sp_ba = case_when(any(area_type == "extended", na.rm = TRUE) ~ 
                                     first(quercus_sp_ba[area_type == "core"]) + first(quercus_sp_ba[area_type == "extended"]),
                                   any(area_type == "core", na.rm = TRUE) ~ 
                                     first(quercus_sp_ba[area_type == "core"]),
                                   TRUE ~ first(quercus_sp_ba))) %>%
  ungroup() %>%
  filter(area_type != "extended" | is.na(area_type))

#Create oak count datasets, both including and excluding trunk shoots
oaks_all_ext <- regeneration_extended %>%
  filter(species == "Quercus sp.") %>%
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>%
  summarise(oak_count = sum(density),
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba),
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")

oaks_noshoots_ext <- regeneration_extended %>%
  filter(species == "Quercus sp.") %>%
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>%
  summarise(oak_count = sum(density[shoot == FALSE]),
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba),
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")



