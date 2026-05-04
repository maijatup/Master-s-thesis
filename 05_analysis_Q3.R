#27/4/2026
#Question 3: How do these factors (basal area, canopy openness, pH) together influence long-term oak regeneration following conservation thinning, compared to the undisturbed control plots?

library(readr)
library(dplyr)
library(glmmTMB)
library(MuMIn)
library(DHARMa)
library(emmeans)

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


#Create a period dataset, excluding trunk shoots
#Merge 2003 and 2005 to early period, calculate means of oak count and canopy openness
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



#Model 1: test the effect of thinning on oak seedling density
m1 <- glmmTMB(oak_count ~ treatment + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m1)
#A significant difference in oak seedling density between early and late period



#Model 2: test the effect of thinning and basal area together
m2 <- glmmTMB(oak_count ~ treatment + total_ba + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m2)
#As basal area increases, oak density decreases (significant)
#No significant difference between periods anymore when ba is accounted for

#Model 2 with oak ba
m2_oak <- glmmTMB(oak_count ~ treatment + quercus_sp_ba + period
                  + offset(log(area_m2))
                  + (1 | site/transect/subplot),
                  family = nbinom2, data = oaks_period)

summary(m2_oak)
#Oak ba doesn't have a significant effect



#Model 3: test the effect of thinning, basal area and canopy together
m3 <- glmmTMB(oak_count ~ treatment + total_ba + canopy_openness + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m3)
#As basal area increases, oak density decreases (significant)
#Period effect is significant again



#Model 4: test the effect of thinning, basal area, canopy and pH together
m4 <- glmmTMB(oak_count ~ treatment + total_ba + canopy_openness + pH + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m4)
#As basal area increases, oak density decreases (significant)
#Significant period effect, much stronger when pH is added, possibly driven by pH differences between periods



#Exclude pH from the final model
#Model 5: test if the effect of basal area differs between treatments
m5 <- glmmTMB(oak_count ~ treatment * total_ba + canopy_openness + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m5)
#In control plots, basal area has a negative effect on oak density, but in thinned plots, basal area has almost no effect


#Test if including trunk shoots influences the results
oaks_period_all <- oaks_all %>%
  mutate(period = if_else(year %in% c(2003, 2005), "early", "late"),
         period = factor(period)) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(oak_count = round2(mean(oak_count)),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE), 
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>% 
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

m5_shoots <- glmmTMB(oak_count ~ treatment * total_ba + canopy_openness + period
                     + offset(log(area_m2))
                     + (1 | site/transect/subplot),
                     family = nbinom2, data = oaks_period_all)

summary(m5_shoots)
#Doesn't affect the results


r.squaredGLMM(m5)

#Diagnostics
#First, create a dataset with only the 94 observations used in the model
oaks_ba_can <- oaks_period %>% 
  filter(complete.cases(total_ba, canopy_openness))

sim_res <- simulateResiduals(m5)

plot(sim_res)
#No significant deviation

testDispersion(sim_res)
testZeroInflation(sim_res)
#No overdispersion or zeroinflation

plotResiduals(sim_res, oaks_ba_can$total_ba)
plotResiduals(sim_res, oaks_ba_can$canopy_openness)
#Minor quantile deviations - due to skewed distribution?

plotResiduals(sim_res, oaks_ba_can$treatment)
#More variance in thinned plots

plotResiduals(sim_res, oaks_ba_can$period)



#Model 6: test if the effect of canopy differs between treatments
m6 <- glmmTMB(oak_count ~ treatment * canopy_openness + total_ba + period
              + offset(log(area_m2))
              + (1 | site/transect/subplot),
              family = nbinom2, data = oaks_period)

summary(m6)
#Canopy has a positive (non-significant) effect in control plots, and a negative effect in thinned plots
#The effect of canopy differs significantly between treatments

#Diagnostics
sim_res6 <- simulateResiduals(m6)

plot(sim_res6)
#Ok but worse than m5

testDispersion(sim_res6)
testZeroInflation(sim_res6)
#No overdispersion or zeroinflation

plotResiduals(sim_res6, oaks_ba_can$total_ba)
#Strong wavy pattern, much worse than m5

plotResiduals(sim_res6, oaks_ba_can$canopy_openness)
#Similar to m5

plotResiduals(sim_res6, oaks_ba_can$treatment)
plotResiduals(sim_res6, oaks_ba_can$period)
#Ok



#Post-hocs
#Estimated marginal means for treatment effect
(emm_treatment <- emmeans(m5, ~ treatment, type = "response"))
pairs(emm_treatment)
#Thinned plots have more oak seedlings (not significant)


#Estimated marginal means for period effect
(emm_period <- emmeans(m5, ~ period, type = "response"))
pairs(emm_period)
#Late period has fewer seedlings than early period (not significant)


#Evaluate treatment effect at low, intermediate, and high basal areas
ba_values <- quantile(oaks_ba_can$total_ba, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

(emm_interaction <- emmeans(m5, ~ treatment | total_ba,
                           at = list(total_ba = ba_values),
                           type = "response"))
pairs(emm_interaction)
#Thinned plots at high ba have significantly higher seedling density

#Effect of basal area within each treatment
emtrends(m5, ~ treatment, var = "total_ba") %>% 
  test()
#In control plots, basal area has a significant negative effect on oak density
#In thinned plots, basal area has no significant effect

