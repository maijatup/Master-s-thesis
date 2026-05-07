#4/5/2026
#Question 4: How does competition from other tree species affect oak seedling density?

library(readr)
library(dplyr)
library(glmmTMB)
library(flextable)
library(DHARMa)
library(lme4)
library(lmerTest)
library(ggplot2)

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
#Choose Sorbus aucuparia, Corylus avellana, Frangula alnus, Populus tremula, Fraxinus excelsior, Picea abies and merged Betula spp. (highest occurrences)


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
#Round oak counts to be able to use nbinom2, round2 for correct rounding
round2 <- function(x) floor(x + 0.5)

competition_p <- competition %>%
  mutate(period = factor(if_else(year %in% c(2003, 2005), "early", "late"))) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(quercus_sp = round2(mean(quercus_sp)),
            fraxinus_excelsior = mean(fraxinus_excelsior),
            frangula_alnus = mean(frangula_alnus),
            sorbus_aucuparia = mean(sorbus_aucuparia),
            populus_tremula = mean(populus_tremula),
            corylus_avellana = mean(corylus_avellana),
            picea_abies = mean(picea_abies),
            betula_sp = mean(betula_sp),
            total_competitor = mean(total_competitor),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE),
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))


competition_all_p <- competition_all %>%
  mutate(period = factor(if_else(year %in% c(2003, 2005), "early", "late"))) %>%
  group_by(site, plot, treatment, transect, subplot, area_m2, period) %>%
  summarise(quercus_sp = round2(mean(quercus_sp)),
            fraxinus_excelsior = mean(fraxinus_excelsior),
            frangula_alnus = mean(frangula_alnus),
            sorbus_aucuparia = mean(sorbus_aucuparia),
            populus_tremula = mean(populus_tremula),
            corylus_avellana = mean(corylus_avellana),
            picea_abies = mean(picea_abies),
            betula_sp = mean(betula_sp),
            total_competitor = mean(total_competitor),
            canopy_openness = mean(canopy_openness, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            total_ba = mean(total_ba, na.rm = TRUE),
            quercus_sp_ba = mean(quercus_sp_ba, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))



#Model 1: test the effect of total competitor density on oak density
m1 <- glmmTMB(quercus_sp ~ total_competitor + period
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m1)
#Total competitor density has a significant positive effect on oak density

#Test if including trunk shoots influences the results
m1_shoots <- glmmTMB(quercus_sp ~ total_competitor + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     family = nbinom2, data = competition_all_p)

summary(m1_shoots)
#Doesn't affect the results



#Model 2: test if the effect of total competitor density differs between periods
m2 <- glmmTMB(quercus_sp ~ total_competitor * period
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m2)
#The effect doesn't differ between periods



#Model 3: test the effect of total competitor density and thinning together
m3 <- glmmTMB(quercus_sp ~ total_competitor + period + treatment
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m3)
#The effect of competitor density stays significant
#Period effect now significant
#Treatment doesn't have a significant effect



#Model 4: test if the effect of total competitor density differs between treatments
m4 <- glmmTMB(quercus_sp ~ total_competitor * treatment + period
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m4)
#The effect doesn't differ between treatments



#Model 5: test the effect of competitor density and basal area together
m5 <- glmmTMB(quercus_sp ~ total_competitor + period + total_ba
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m5)
#Total basal area has a negative effect, the effect of total competitor density disappears - ba a stronger driver?
#Note less data than m1

#Run m1 with the same subset of data as m5 to compare
m1_subset <- glmmTMB(quercus_sp ~ total_competitor + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     family = nbinom2,
                     data = subset(competition_p, !is.na(total_ba)))

summary(m1_subset)
#The effect of total competitor density disappears due to less data and not basal area



#Test the effects of individual species densities
#Test the effect of Sorbus aucuparia density on oak density
m_sorbus <- glmmTMB(quercus_sp ~ sorbus_aucuparia + period
                    + offset(log(area_m2))
                    + (1 | site/plot/transect/subplot),
                    family = nbinom2, data = competition_p)

summary(m_sorbus)
#No significant effect on oak density



#Test the effect of Corylus avellana on oak density
m_corylus <- glmmTMB(quercus_sp ~ corylus_avellana + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     family = nbinom2, data = competition_p)

summary(m_corylus)
#No significant effect on oak density



#Test the effect of Frangula alnus on oak density
m_frangula <- glmmTMB(quercus_sp ~ frangula_alnus + period
                      + offset(log(area_m2))
                      + (1 | site/plot/transect/subplot),
                      family = nbinom2, data = competition_p)

summary(m_frangula)
#No significant effect on oak density



#Test the effect of Populus tremula on oak density
m_populus <- glmmTMB(quercus_sp ~ populus_tremula + period
                     + offset(log(area_m2))
                     + (1 | site/plot/transect/subplot),
                     family = nbinom2, data = competition_p)

summary(m_populus)
#No significant effect on oak density



#Test the effect of Fraxinus excelsior on oak density
m_fraxinus <- glmmTMB(quercus_sp ~ fraxinus_excelsior + period
                      + offset(log(area_m2))
                      + (1 | site/plot/transect/subplot),
                      family = nbinom2, data = competition_p)

summary(m_fraxinus)
#Fraxinus excelsior has a significant positive effect on oak density



#Test the effect of Picea abies on oak density
m_picea <- glmmTMB(quercus_sp ~ picea_abies + period
                   + offset(log(area_m2))
                   + (1 | site/plot/transect/subplot),
                   family = nbinom2, data = competition_p)

summary(m_picea)
#No significant effect



#Test the effect of Betula sp. on oak density
m_betula <- glmmTMB(quercus_sp ~ betula_sp + period
                    + offset(log(area_m2))
                    + (1 | site/plot/transect/subplot),
                    family = nbinom2, data = competition_p)

summary(m_betula)
#Betula sp. have a significant negative effect on oak density


#Check the distribution of Fraxinus, Betula and Quercus across the plots
competition_p %>%
  group_by(site, treatment) %>%
  summarise(fraxinus = sum(fraxinus_excelsior),
            betula = sum(betula_sp),
            quercus = sum(quercus_sp), .groups = "drop")
#Both concentrated in one site or plot, the significant effects could be due to this

#Make a table that shows these distributions
table1 <- competition_p %>%
  group_by(site, treatment) %>%
  summarise(fraxinus = sum(fraxinus_excelsior),
            betula = sum(betula_sp),
            quercus = sum(quercus_sp), .groups = "drop") %>%
  rename(Site = site, Treatment = treatment,
         "Fraxinus excelsior" = fraxinus,
         "Betula sp." = betula,
         "Quercus sp." = quercus) %>%
  flextable() %>%
  merge_v(j = "Site") %>%
  theme_booktabs() %>%
  autofit() %>%
  italic(j = c("Fraxinus excelsior", "Betula sp.", "Quercus sp."), part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  colformat_num(big.mark = "", decimal.mark = ".", digits = 1)

save_as_docx(table1, path = "competition_table.docx")



#Calculate competitor species richness
competitor_richness <- regeneration_core %>%
  filter(shoot == FALSE, species != "Quercus sp.") %>%
  group_by(site, plot, treatment, year, transect, subplot) %>%
  summarise(competitor_richness = n_distinct(species), .groups = "drop") %>%
  mutate(period = factor(if_else(year %in% c(2003, 2005), "early", "late"))) %>%
  group_by(site, plot, treatment, transect, subplot, period) %>%
  summarise(competitor_richness = mean(competitor_richness), .groups = "drop")

competition_p <- competition_p %>%
  left_join(competitor_richness, by = c("site", "plot", "treatment", "transect", "subplot", "period")) %>% 
  mutate(competitor_richness = ifelse(is.na(competitor_richness), 0, competitor_richness))


#Model 6: test the effect of competitor species richness on oak density
m6 <- glmmTMB(quercus_sp ~ competitor_richness + period
              + offset(log(area_m2))
              + (1 | site/plot/transect/subplot),
              family = nbinom2, data = competition_p)

summary(m6)
#Competitor species richness doesn't have a significant effect



#Diagnostics
sim_res <- simulateResiduals(m1)
plot(sim_res)
#Model not optimal but still OK?




#Model competition using height data instead of density
height_data <- read_csv2("raw_data/seedling_data_raw.csv")

#Add a shoot column
height_data <- height_data %>% 
  mutate(shoot = notes %in% c("stubbskott, räknas ej?",
                              "stubbskott",
                              "stubbskott på ovan rönn",
                              "stubbskott av nian")) %>% 
  select(site, plot, treatment, transect, subplot, species, height_cm, diameter_cm, shoot)


#Calculate mean heights and height ratio
#Filter out everything >130 cm (exact height not measured)
height_index <- height_data %>%
  filter(species != "0", !is.na(height_cm), height_cm != ">130") %>%
  mutate(height_cm = as.numeric(height_cm),
         height_cm = ifelse(height_cm > 130, NA, height_cm)) %>%
  filter(!is.na(height_cm), shoot == FALSE) %>%
  group_by(site, plot, treatment, transect, subplot) %>%
  summarise(mean_oak_height = mean(height_cm[species == "Quercus sp."], na.rm = TRUE),
            mean_competitor_height = mean(height_cm[species != "Quercus sp."], na.rm = TRUE), .groups = "drop") %>%
  mutate(height_ratio = mean_competitor_height / mean_oak_height)

#Join to main dataset, note that exact height values only exist for 2025
#Create subplot id to be able to remove transect from nesting so the model will converge
competition_p <- competition_p %>%
  left_join(height_index %>% select(site, plot, treatment, transect, subplot, height_ratio, mean_oak_height),
            by = c("site", "plot", "treatment", "transect", "subplot")) %>%
  mutate(height_ratio = ifelse(period == "early", NA, height_ratio),
         mean_oak_height = ifelse(period == "early", NA, mean_oak_height),
         subplot_id = paste(site, transect, subplot, sep = "_"))



#Test the effect of mean competitor height on mean oak height
m_height2 <- lmer(mean_oak_height ~ mean_competitor_height 
                  + (1 | site/plot),
                  data = height_index)

summary(m_height2)
#Mean competitor height has a significant positive effect on mean oak height

res_mh2 <- simulateResiduals(m_height2)
plot(res_mh2)
#Significant deviation

#Log transform, create unique plot IDs
height_index <- height_index %>%
  mutate(plot_id = paste(site, plot, sep = "_")) %>% 
  mutate(subplot_id = paste(site, transect, subplot, sep = "_"))

m_height2_log <- lmer(log(mean_oak_height) ~ mean_competitor_height 
                      + (1 | plot_id),
                      data = height_index)

summary(m_height2_log)
#Mean competitor height has a significant effect on mean oak height

res_mh2log <- simulateResiduals(m_height2_log)
plot(res_mh2log)
#No significant deviation



#Test the effect of mean competitor height and treatment together
m_height3 <- lmer(mean_oak_height ~ mean_competitor_height + treatment
                  + (1 | site/plot),
                  data = height_index)

summary(m_height3)

res_mh3 <- simulateResiduals(m_height3)
plot(res_mh3)
#Significant deviation

#Log transform
m_height3_log <- lmer(log(mean_oak_height) ~ mean_competitor_height + treatment
                      + (1 | plot_id),
                      data = height_index)

summary(m_height3_log)
#Treatment doesn't have a significant effect

res_mh3log <- simulateResiduals(m_height3_log)
plot(res_mh3log)
#No significant deviation

#Test the effect of total competitor density on mean oak height
competition_p <- competition_p %>%
  mutate(plot_id = paste(site, plot, sep = "_"))

m_height4 <- lmer(log(mean_oak_height) ~ total_competitor
                  + (1 | plot_id),
                  data = competition_p)

summary(m_height4)
#Total competitor density doesn't have a significant effect on mean oak height

res_mh4 <- simulateResiduals(m_height4)
plot(res_mh4)
#No significant deviation


#Test if treatment affects oak height
oak_heights <- height_data %>%
  filter(species == "Quercus sp.", shoot == FALSE,
         !is.na(height_cm), height_cm != ">130") %>%
  mutate(height_cm = as.numeric(height_cm),
         subplot_id = paste(site, transect, subplot, sep = "_")) %>%
  filter(height_cm <= 130)

m_oakheight <- lmer(height_cm ~ treatment
                    + (1 | site/subplot_id),
                    data = oak_heights)

summary(m_oakheight)
#Treatment has a significant positive effect on oak seedling height

res_oakheight <- simulateResiduals(m_oakheight)
plot(res_oakheight)
#Significant deviation

#Log transform
m_oakheight_log <- lmer(log(height_cm) ~ treatment
                        + (1 | site/subplot_id),
                        data = oak_heights)

summary(m_oakheight_log)

res_oakheight_log <- simulateResiduals(m_oakheight_log)
plot(res_oakheight_log)
#Still significant deviation

#Test if treatment affects competitor height
competitor_heights <- height_data %>%
  filter(species != "Quercus sp.", species != "0", shoot == FALSE,
         !is.na(height_cm), height_cm != ">130") %>%
  mutate(height_cm = as.numeric(height_cm),
         subplot_id = paste(site, transect, subplot, sep = "_")) %>%
  filter(height_cm <= 130)

m_compheight <- lmer(height_cm ~ treatment
                     + (1 | site/subplot_id),
                     data = competitor_heights)

summary(m_compheight)
#Treatment has a significant positive effect on competitor seedling height
#Competitor seedlings are on average taller than oak seedlings

res_compheight <- simulateResiduals(m_compheight)
plot(res_compheight)
#Significant deviation

#Log transform
m_compheight_log <- lmer(log(height_cm) ~ treatment
                         + (1 | site/subplot_id),
                         data = competitor_heights)

summary(m_compheight_log)

res_compheight_log <- simulateResiduals(m_compheight_log)
plot(res_compheight_log)
#Still significant deviation, heights are much more variable in thinned plots?
#Just accept this as a limitation?


#Test height ratio (mean competitor height / mean oak height)
#A ratio > 1 means competitors are on average taller than oaks
#This directly tests whether competitors outpace oaks, rather than both just tracking habitat quality

height_index <- height_index %>%
  mutate(plot_id = paste(site, plot, sep = "_"),
         subplot_id = paste(site, transect, subplot, sep = "_"))

#Model: does treatment affect the height ratio?
m_ratio1 <- lmer(log(height_ratio) ~ treatment + (1 | plot_id),
                 data = height_index)

summary(m_ratio1)
#If thinning reduces the ratio, oaks are keeping up with competitors better in thinned plots

res_ratio1 <- simulateResiduals(m_ratio1)
plot(res_ratio1)


#Join total competitor density (late period only) to height_index
height_index_comp <- competition_p %>%
  filter(period == "late") %>%
  select(site, plot, treatment, transect, subplot, total_competitor) %>%
  right_join(height_index, by = c("site", "plot", "treatment", "transect", "subplot"))

#Model: does total competitor density relate to the height ratio?
m_ratio2 <- lmer(log(height_ratio) ~ total_competitor + (1 | plot_id),
                 data = height_index_comp)

summary(m_ratio2)
#If more competitors = higher ratio, competitors are outpacing oaks in denser subplots

res_ratio2 <- simulateResiduals(m_ratio2)
plot(res_ratio2)


#Model: does the height ratio predict oak density?
#Most direct test: are subplots where competitors are taller relative to oaks associated with fewer oaks?
m_ratio3 <- glmmTMB(quercus_sp ~ height_ratio + offset(log(area_m2))
                    + (1 | site/plot),
                    family = nbinom2,
                    data = competition_p)

summary(m_ratio3)

res_ratio3 <- simulateResiduals(m_ratio3)
plot(res_ratio3)
