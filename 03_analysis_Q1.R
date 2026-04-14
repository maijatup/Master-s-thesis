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

oaks_noshoots <- regeneration_core %>% 
  filter(species == "Quercus sp.",
         shoot == FALSE) %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
  summarise(oak_count = sum(density), .groups = "drop")



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
#Overdispersion detected -> switch to negative binomial

check_model(m3_p, check = "homogeneity")
#This is the only thing in check_model() that gave a result
#Should I run other diagnostics??



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
#Oak density increases by 0.9% per year in thinned plots (just significantly different from control?)


#Try the model with year as factor
m4_year_f <- glmmTMB(oak_count ~ treatment * factor(year) + offset(log(area_m2))
                     + (1 | site/transect/subplot),
                     data = oaks_all, family = nbinom2)

summary(m4_year_f)
#In 2025, oak density is 57% lower than in 2003 (significant)
#The effect of thinning became significantly more positive over time
#Thinned plots show higher oak regeneration than control plots in 2025




