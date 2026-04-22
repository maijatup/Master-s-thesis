#21/4/2026
#Question 2: How do forest structure, light availability, and soil conditions influence oak seedling density across the study sites?

library(readr)
library(dplyr)
library(glmmTMB)

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
#Add areas of ba measurements and then calculate weighted average basal areas
regeneration_extended <- regeneration %>%
  mutate(area_ha = case_when(
    area_type == "core" & site %in% c("Karla", "Rya åsar", "Östadkulle") ~ 0.249,
    area_type == "core" & site %in% c("Sandviksås", "Skölvene") ~ 0.3,
    area_type == "extended" & site == "Karla" & plot == "SO" ~ 0.332,
    area_type == "extended" & site == "Rya åsar" & plot == "NO" ~ 0.083,
    area_type == "extended" & site == "Sandviksås" & plot == "Ö" ~ 0.2,
    area_type == "extended" & site == "Skölvene" & plot == "Ö" ~ 0.3,
    area_type == "extended" & site == "Skölvene" & plot == "V" ~ 0.1,
    area_type == "extended" & site == "Östadkulle" & plot == "Ö" ~ 0.249)) %>%
  group_by(site, plot, treatment, year, transect, subplot) %>%
  mutate(
    total_ba = case_when(any(area_type == "extended", na.rm = TRUE) ~ 
        (first(total_ba[area_type == "core"]) * first(area_ha[area_type == "core"]) + 
           first(total_ba[area_type == "extended"]) * first(area_ha[area_type == "extended"])) / 
        (first(area_ha[area_type == "core"]) + first(area_ha[area_type == "extended"])),
        any(area_type == "core", na.rm = TRUE) ~ 
          first(total_ba[area_type == "core"]),
        TRUE ~ first(total_ba)),
    quercus_sp_ba = case_when(any(area_type == "extended", na.rm = TRUE) ~ 
        (first(quercus_sp_ba[area_type == "core"]) * first(area_ha[area_type == "core"]) + 
           first(quercus_sp_ba[area_type == "extended"]) * first(area_ha[area_type == "extended"])) / 
        (first(area_ha[area_type == "core"]) + first(area_ha[area_type == "extended"])),
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



#Use oaks_noshoots as the main dataset as it is more ecologically correct
#Model 1: test the effect of total basal area on oak seedling density
m1 <- glmmTMB(oak_count ~ total_ba + factor(year)
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              data = oaks_noshoots, family = nbinom2)

summary(m1)
#As basal area increases, oak seedling density decreases (significant)

#Test if including ba measurements from the extended area influences the results
m1_ext <- glmmTMB(oak_count ~ total_ba + factor(year)
                  + offset(log(area_m2))
                  + (1 | site/plot/transect/subplot),
                  data = oaks_noshoots_ext, family = nbinom2)

summary(m1_ext)
#Similar results to m1, even though this model has slightly stronger effect, which makes sense since the basal area values are slightly larger -> robust results

#Test if including trunk shoots influences the results
m1_shoots <- glmmTMB(oak_count ~ total_ba + factor(year)
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m1_shoots)
#Doesn't affect the results

#Test the effect of oak basal area on oak seedling density
m1_oak <- glmmTMB(oak_count ~ quercus_sp_ba + factor(year)
                  + offset(log(area_m2))
                  + (1 | site/plot/transect/subplot),
                  data = oaks_noshoots, family = nbinom2)

summary(m1_oak)
#Similar, significant result, but slightly stronger effect



#Model 2: test the effect of canopy openness on oak seedling density
m2 <- glmmTMB(oak_count ~ canopy_openness + factor(year)
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              data = oaks_noshoots, family = nbinom2)

summary(m2)
#Canopy openness doesn’t have a significant effect on oak seedling density

#Test if including trunk shoots influences the results
m2_shoots <- glmmTMB(oak_count ~ canopy_openness + factor(year)
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m2_shoots)
#Doesn't affect the results



#Model 3: test the effect of pH on oak seedling density
m3 <- glmmTMB(oak_count ~ pH + factor(year)
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              data = oaks_noshoots, family = nbinom2)

summary(m3)
#pH doesn't have a significant effect on oak seedling density

#Test if including trunk shoots influences the results
m3_shoots <- glmmTMB(oak_count ~ pH + factor(year)
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m3_shoots)
#Doesn't affect the results



#Model 4: test the effect of all predictors together
#Note: 2025 is the only year with all predictor values
m4 <- glmmTMB(oak_count ~ total_ba + canopy_openness + pH
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              data = oaks_noshoots, family = nbinom2)

summary(m4)
#Note very small sample size
#Similar results, as basal area increases, oak density decreases (significant)

#Test if including trunk shoots influences the results
m4_shoots <- glmmTMB(oak_count ~ total_ba + canopy_openness + pH
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m4_shoots)
#Doesn't affect the results


