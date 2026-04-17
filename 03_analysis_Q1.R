#30/3/2026
#Question 1: Do the positive effects of conservation thinning on oak regeneration persist 22 years after thinning?

library(readr)
library(dplyr)
library(glmmTMB)
library(performance)

regeneration <- read_csv("processed_data/regeneration_data.csv")

#Filter to exclude area_type "extended" to remove duplicate seedling rows
regeneration_core <- regeneration %>% 
  filter(area_type != "extended" | is.na(area_type))


#Create oak count datasets, both including and excluding trunk shoots
oaks_all <- regeneration_core %>% 
  filter(species == "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density), .groups = "drop")

#Note: exclude shoots inside the sum so zero-oak seedling subplots are kept
oaks_noshoots <- regeneration_core %>% 
  filter(species == "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density[shoot == FALSE]), .groups = "drop")



#Model 1: test effect of thinning on oak seedling density
#Area as an offset because some subplots were 3m2 instead of 5m2
m1_p <- glm(oak_count ~ treatment + offset(log(area_m2)),
            data = oaks_all, family = poisson)

summary(m1_p)
#Oak density is slightly higher (exp(0.08322) = 1.087 - 8.7% higher) in treatment plots, but treatment alone explains very little variation


#Model 1 without offset for comparison
m1_x <- glm(oak_count ~ treatment,
          data = oaks_all, family = poisson)

summary(m1_x)
#AIC 4948.5 higher than m1_p AIC 4932.3 -> m1_p is better


#Add random effects
m1_pr <- glmmTMB(oak_count ~ treatment + offset(log(area_m2)) 
                 + (1 | site/transect/subplot),
                 data = oaks_all, family = poisson)

summary(m1_pr)
#No significant effect
#A lot of spatial variation, oak density differs a lot between subplots


#Model 2: test if oak density changes over time (year effect)
m2_p <- glmmTMB(oak_count ~ year + offset(log(area_m2)) 
                + (1 | site/transect/subplot),
                data = oaks_all, family = poisson)

summary(m2_p)
#Oak density decreases by 1.5% per year (significant), but there's a lot of spatial variation again


#Model 3: test additive effects of thinning and year on oak density
m3_p <- glmmTMB(oak_count ~ treatment + year + offset(log(area_m2))
                + (1 | site/transect/subplot),
                data = oaks_all, family = poisson)

summary(m3_p)
#Oak density decreases 1.5% per year (significant), but thinning has no significant effect on oak density


#Check model diagnostics
check_overdispersion(m1_pr)
check_overdispersion(m2_p)
check_overdispersion(m3_p)
check_model(m3_p)
#Overdispersion detected -> switch to negative binomial




#Model 1 with negative binomial distribution
m1_nb <- glmmTMB(oak_count ~ treatment + offset(log(area_m2))
                 + (1 | site/transect/subplot),
                 data = oaks_all, family = nbinom2)

summary(m1_nb)
#No significant effect, strong spatial variation
#Using negative binomial distribution improved the model by lowering the AIC value (1762.8 -> 1380.2), overdispersion also decreased


#Model 2 with negative binomial distribution
m2_nb <- glmmTMB(oak_count ~ year + offset(log(area_m2))
                 + (1 | site/transect/subplot),
                 data = oaks_all, family = nbinom2)

summary(m2_nb)
#Oak density decreases by 1.5% per year, but the effect is not significant


#Model 3 with negative binomial distribution
m3_nb <- glmmTMB(oak_count ~ treatment + year + offset(log(area_m2))
                 + (1 | site/transect/subplot),
                 data = oaks_all, family = nbinom2)

summary(m3_nb)
#Treatment doesn't have any significant effects, oak density decreases by 1.5% per year (significant)


#Model 4: test if the effect of thinning changes over time (interaction)
m4 <- glmmTMB(oak_count ~ treatment * year + offset(log(area_m2))
              + (1 | site/transect/subplot),
              data = oaks_all, family = nbinom2)
#Model convergence problem - the model cannot reliably estimate the interaction effect
#Maybe due to high year values?


#Try centering year and see if this affects the model
oaks_all <- oaks_all %>% 
  mutate(year_c = year - mean(year))

m4_year_c <- glmmTMB(oak_count ~ treatment * year_c + offset(log(area_m2))
                     + (1 | site/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m4_year_c)
#Now the intercept is meaningful
#Oak density decreases by 3.5% per year in control plots (significant)
#Thinned plots show a significantly more positive trend (0.9% increase per year)


#Try the model with year as factor
m4_year_f <- glmmTMB(oak_count ~ treatment * factor(year) + offset(log(area_m2))
                     + (1 | site/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m4_year_f)
#In 2025, oak density is 57% lower in control plots than in 2003 (significant)
#Thinned plots show significantly higher oak regeneration than control plots in 2025


#Diagnostics
check_model(m4_year_c)
check_model(m4_year_f)
#Mismatch between observed and predicted variance

#Check zero-inflation
check_zeroinflation(m4_year_c)
check_zeroinflation(m4_year_f)
#Not a big difference


#Test models with nbinom1 distribution, which assumes a more linear mean–variance relationship (nbinom2 assumes quadratic)
m4_nb1_c <- glmmTMB(oak_count ~ treatment * year_c + offset(log(area_m2))
                    + (1 | site/transect/subplot),
                    data = oaks_all, family = nbinom1)
summary(m4_nb1_c)


m4_nb1_f <- glmmTMB(oak_count ~ treatment * factor(year) + offset(log(area_m2))
                    + (1 | site/transect/subplot),
                    data = oaks_all, family = nbinom1)
summary(m4_nb1_f)

#Check overdispersion
check_overdispersion(m4_year_c)
check_overdispersion(m4_year_f)
check_overdispersion(m4_nb1_c)
check_overdispersion(m4_nb1_f)
#No overdispersion detected, but nb1 ratios closer to 1

check_model(m4_nb1_c)
check_model(m4_nb1_f)
#No big differences to nbinom2 plots, but higher residual variance in the dispersion and zero-inflation plots

#Compare models
AIC(m4_year_c, m4_year_f, m4_nb1_c, m4_nb1_f)
compare_performance(m4_year_c, m4_year_f, m4_nb1_c, m4_nb1_f, rank = TRUE, verbose = FALSE)
#Most variation in oak regeneration comes from differences between sites, transects and subplots
#Nbinom1 models have lower AIC and higher AIC weight -> higher probability of being the best model, also highest performance scores



#Continue with the factor(year) nbinom2 model
#Add plot in the random effects
m4_year_f_plot <- glmmTMB(oak_count ~ treatment * factor(year)
                          + offset(log(area_m2))
                          + (1 | site/plot/transect/subplot),
                          data = oaks_all, family = nbinom2)

summary(m4_year_f_plot)
#Similar results to m4_year_f -> including plot did not substantially change the fixed effect estimates
#A lot of the variation attributed to transect in the previous model m4_year_f was actually due to differences between plots
#Most variation is still on subplot and site levels


#Merge 2003 & 2005 to early period after thinning, 2025 is late period
oaks_all <- oaks_all %>% 
  mutate(period = if_else(year %in% c(2003, 2005), "early", "late"),
         period = factor(period))

m4_period <- glmmTMB(oak_count ~ treatment * period
                     + offset(log(area_m2))
                     + (1 | site/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m4_period)
#In the late period, density in control plots is 52% lower than in the early period (significant)
#The effect of thinning changes significantly over time


#Combine these two models
m4_period_plot <- glmmTMB(oak_count ~ treatment * period
                          + offset(log(area_m2))
                          + (1 | site/plot/transect/subplot),
                          data = oaks_all, family = nbinom2)

summary(m4_period_plot)
#Similar results and AIC values to the previous two models


#Run this model with oaks_noshoots to test if excluding trunk shoots affects the results
oaks_noshoots <- oaks_noshoots %>% 
  mutate(period = if_else(year %in% c(2003, 2005), "early", "late"),
         period = factor(period))

m4_noshoots <- glmmTMB(oak_count ~ treatment * period
                       + offset(log(area_m2))
                       + (1 | site/plot/transect/subplot),
                       data = oaks_noshoots, family = nbinom2)

summary(m4_noshoots)
#Similar results, but the interaction effect slightly increased -> the thinning effect is possibly slightly stronger when considering only true oak seedlings
#Use this for the main analysis since it's more ecologically correct

