#4/5/2026
#How does competition from other tree species affect oak seedling density?

library(readr)
library(dplyr)

regeneration <- read_csv("processed_data/regeneration_data.csv")

#Filter to exclude area_type "extended" to remove duplicate seedling rows
regeneration_core <- regeneration %>% 
  filter(area_type != "extended" | is.na(area_type))


#Decide which species to include in the competition dataset
#Summarise species by total density and occurrence (n and % of subplot-years)
regeneration_core %>%
  group_by(species) %>%
  summarise(total_density = sum(density, na.rm = TRUE),
            n_subplot_years = n_distinct(paste(site, plot, transect, subplot, year)), .groups = "drop") %>%
  mutate(p_subplot_years = 100 * n_subplot_years / 224) %>%
  arrange(desc(total_density)) %>%
  print(n = Inf)


#Create competition datasets, both without (main) and with trunk shoots
competition <- regeneration_core %>%
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>%
  summarise(quercus_sp = sum(density[shoot == FALSE & species == "Quercus sp."], na.rm = TRUE),
            fraxinus_excelsior = sum(density[shoot == FALSE & species == "Fraxinus excelsior"], na.rm = TRUE),
            frangula_alnus = sum(density[shoot == FALSE & species == "Frangula alnus"], na.rm = TRUE),
            sorbus_aucuparia = sum(density[shoot == FALSE & species == "Sorbus aucuparia"], na.rm = TRUE),
            populus_tremula = sum(density[shoot == FALSE & species == "Populus tremula"], na.rm = TRUE),
            corylus_avellana = sum(density[shoot == FALSE & species == "Corylus avellana"], na.rm = TRUE),
            picea_abies = sum(density[shoot == FALSE & species == "Picea abies"], na.rm = TRUE),
            betula_sp = sum(density[shoot == FALSE & species %in% c("Betula sp.", "Betula pendula", "Betula pubescens")], na.rm = TRUE),
            total_competitor = sum(density[shoot == FALSE & species != "Quercus sp."], na.rm = TRUE),
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba),
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")


competition_all <- regeneration_core %>%
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>%
  summarise(quercus_sp = sum(density[species == "Quercus sp."], na.rm = TRUE),
            fraxinus_excelsior = sum(density[species == "Fraxinus excelsior"], na.rm = TRUE),
            frangula_alnus = sum(density[species == "Frangula alnus"], na.rm = TRUE),
            sorbus_aucuparia = sum(density[species == "Sorbus aucuparia"], na.rm = TRUE),
            populus_tremula = sum(density[species == "Populus tremula"], na.rm = TRUE),
            corylus_avellana = sum(density[species == "Corylus avellana"], na.rm = TRUE),
            picea_abies = sum(density[species == "Picea abies"], na.rm = TRUE),
            betula_sp = sum(density[species %in% c("Betula sp.", "Betula pendula", "Betula pubescens")], na.rm = TRUE),
            total_competitor = sum(density[species != "Quercus sp."], na.rm = TRUE),
            canopy_openness = first(canopy_openness),
            pH = first(pH),
            total_ba = first(total_ba),
            quercus_sp_ba = first(quercus_sp_ba), .groups = "drop")


#Create period datasets by merging 2003&2005
#Round counts to be able to use nbinom2, round2 for correct rounding
round2 <- function(x) floor(x + 0.5)

competition_p <- competition %>%
  mutate(period = factor(if_else(year %in% c(2003, 2005), "early", "late"))) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(quercus_sp = round2(mean(quercus_sp)),
            fraxinus_excelsior = round2(mean(fraxinus_excelsior)),
            frangula_alnus = round2(mean(frangula_alnus)),
            sorbus_aucuparia = round2(mean(sorbus_aucuparia)),
            populus_tremula = round2(mean(populus_tremula)),
            corylus_avellana = round2(mean(corylus_avellana)),
            picea_abies = round2(mean(picea_abies)),
            betula_sp = round2(mean(betula_sp)),
            total_competitor = round2(mean(total_competitor)),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE),
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))


competition_all_p <- competition_all %>%
  mutate(period = factor(if_else(year %in% c(2003, 2005), "early", "late"))) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(quercus_sp = round2(mean(quercus_sp)),
            fraxinus_excelsior = round2(mean(fraxinus_excelsior)),
            frangula_alnus = round2(mean(frangula_alnus)),
            sorbus_aucuparia = round2(mean(sorbus_aucuparia)),
            populus_tremula = round2(mean(populus_tremula)),
            corylus_avellana = round2(mean(corylus_avellana)),
            picea_abies = round2(mean(picea_abies)),
            betula_sp = round2(mean(betula_sp)),
            total_competitor = round2(mean(total_competitor)),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE),
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

