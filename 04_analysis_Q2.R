#21/4/2026
#Question 2: How do forest structure, light availability, and soil conditions influence oak seedling density across the study sites?

library(readr)
library(dplyr)
library(glmmTMB)
library(Hmisc)
library(piecewiseSEM)
library(MuMIn)

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

#Create extended oak count datasets, both including and excluding trunk shoots
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



#Check correlation between the predictors
oaks_noshoots %>%
  filter(!is.na(total_ba) & !is.na(canopy_openness) & !is.na(pH)) %>%
  dplyr::select(total_ba, canopy_openness, pH) %>%
  as.matrix() %>%
  rcorr()
#Significant, moderate correlation between basal area and pH
#But note that this is only using 2025 values, check pairwise correlations

cor.test(oaks_noshoots$total_ba, oaks_noshoots$canopy_openness)
cor.test(oaks_noshoots$total_ba, oaks_noshoots$pH) #again only 2025 data
cor.test(oaks_noshoots$canopy_openness, oaks_noshoots$pH)
#All correlations significant and moderate


#Structural Equation Modeling (SEM) of causal relationships between variables
#Filter only 2025
oaks_2025 <- oaks_noshoots %>% 
  filter(!is.na(total_ba) & !is.na(canopy_openness) & !is.na(pH))

sem_model <- psem(glmmTMB(canopy_openness ~ total_ba
                          + (1 | site),
                          data = oaks_2025,
                          family = gaussian),
                  
                  glmmTMB(total_ba ~ pH
                          + (1 | site),
                          data = oaks_2025,
                          family = gaussian),
                  
                  glmmTMB(oak_count ~ total_ba + canopy_openness + pH
                          + (1 | site/plot/transect/subplot),
                          data = oaks_2025,
                          family = nbinom2))

summary(sem_model) #warning: might need to scale or center?
coefs(sem_model)
#BA affects oak density primarily through a direct pathway
#BA has a significant indirect pathway through pH / pH has a pathway through BA



#Create a dataset with period instead of year
#Merge 2003 and 2005 to early period, calculate means (identical to medians here) of response and predictor variables
#Round oak count to be able to use nbinom2, round2 for correct rounding
round2 <- function(x) floor(x + 0.5)

oaks_period <- oaks_noshoots %>%
  mutate(period = if_else(year %in% c(2003, 2005), "early", "late"),
         period = factor(period)) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(oak_count = round2(mean(oak_count)),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE), 
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>% 
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))


#Run the models again with this dataset
m1_period <- glmmTMB(oak_count ~ total_ba + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_period, family = nbinom2)

summary(m1_period)
#As basal area increases, oak seedling density decreases (significant)


m2_period <- glmmTMB(oak_count ~ canopy_openness + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_period, family = nbinom2)

summary(m2_period)
#Canopy openness doesn’t have a significant effect on oak seedling density
#Oak density decreases significantly over time, regardless of canopy


m3_period <- glmmTMB(oak_count ~ pH + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_period, family = nbinom2)

summary(m3_period)
#pH doesn’t have a significant effect on oak seedling density



m4_period_pH <- glmmTMB(oak_count ~ total_ba + canopy_openness + pH + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     data = oaks_period, family = nbinom2)

summary(m4_period_pH)
#As basal area decreases, oak seedling density decreases (significant)
#Period has a significant negative effect on oak seedling density, but note only 66 observations

#pH was measured using different methods in 2005 and 2025, and cannot be directly compared, therefore dropped from this model
m4_period <- glmmTMB(oak_count ~ total_ba + canopy_openness + period
                        + offset(log(area_m2))
                        + (1 | site/plot/transect/subplot),
                        data = oaks_period, family = nbinom2)

summary(m4_period)
#As basal area decreases, oak seedling density decreases (significant), similar effect size in both m4 models
#The effect size of period is lower in this model, likely due to different pH between periods (higher values in early period, lower in late)



r.squaredGLMM(m1_period)
r.squaredGLMM(m2_period)
r.squaredGLMM(m3_period)
r.squaredGLMM(m4_period)
#m4_period has the highest marginal R2 -> basal area and canopy together explain more variance than any predictor alone
#The random effects explain a large proportion of the variance, oak seedling density varies a lot across sites


cor.test(oaks_period$total_ba, oaks_period$canopy_openness)
#Moderate negative correlation (significant) 

#SEM on the period models
oaks_sem <- oaks_period %>%
  filter(!is.na(total_ba) & !is.na(canopy_openness))

sem_period <- psem(glmmTMB(canopy_openness ~ total_ba
                           + (1 | site/plot/transect/subplot),
                           data = oaks_sem,
                           family = gaussian),
                   
                   glmmTMB(oak_count ~ total_ba + canopy_openness + period
                           + (1 | site/plot/transect/subplot),
                           data = oaks_sem,
                           family = nbinom2))

coefs(sem_period)
#Basal area is the only significant direct predictor of oak density
#As basal area increases, canopy openness slightly decreases
#Significant decline of oak density between early and late period

