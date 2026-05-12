#6/5/2026

#General results

library(readr)
library(dplyr)
library(tidyr)
library(flextable)

regeneration <- read_csv("processed_data/regeneration_data.csv")

#Filter to exclude area_type "extended" to remove duplicate seedling rows
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


#Create a height dataset
height_data <- read_csv2("raw_data/seedling_data_raw.csv")

#Add a shoot column
height_data <- height_data %>% 
  mutate(shoot = notes %in% c("stubbskott, räknas ej?",
                              "stubbskott",
                              "stubbskott på ovan rönn",
                              "stubbskott av nian")) %>% 
  select(site, plot, treatment, transect, subplot, species, height_cm, diameter_cm, shoot)



#Total oak seedling density per m2 across all subplots, by year
oaks_noshoots %>%
  group_by(year) %>%
  summarise(n_subplots = n(),
            total_count = sum(oak_count),
            mean_per_m2 = mean(oak_count / area_m2),
            sd_per_m2 = sd(oak_count / area_m2),
            median_per_m2 = median(oak_count / area_m2))


#Total oak seedling density by site, treatment and year
oaks_noshoots %>%
  group_by(site, treatment, year) %>%
  summarise(n_subplots = n(),
            mean_per_m2 = mean(oak_count / area_m2),
            sd_per_m2 = sd(oak_count / area_m2), .groups = "drop")

#Create a nicer table
oak_table <- oaks_noshoots %>%
  group_by(site, treatment, year) %>%
  summarise(mean_per_m2 = mean(oak_count / area_m2, na.rm = TRUE),
            sd_per_m2 = sd(oak_count / area_m2, na.rm = TRUE), .groups = "drop") %>%
  mutate(density = sprintf("%.2f ± %.2f", mean_per_m2, sd_per_m2)) %>%
  select(site, treatment, year, density) %>%
  pivot_wider(names_from = year,
              values_from = density) %>% 
  rename(Site = site,
         Treatment = treatment,
         "2003 (mean ± SD)" = `2003`,
         "2005 (mean ± SD)" = `2005`,
         "2025 (mean ± SD)" = `2025`)

(oak_density_table <- oak_table %>% 
  flextable() %>%
  merge_v(j = "Site") %>%
  theme_booktabs() %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all"))

save_as_docx(oak_density_table, path = "oak_density_table.docx")


#Total oak seedling density by treatment and year
oaks_noshoots %>%
  group_by(treatment, year) %>%
  summarise(n_subplots = n(),
            mean_per_m2 = mean(oak_count / area_m2),
            sd_per_m2 = sd(oak_count / area_m2), .groups = "drop")


#Early vs late period: oak seedlings by site and treatment
oaks_period %>%
  group_by(site, treatment, period) %>%
  summarise(n_subplots = n(),
            mean_per_m2 = mean(oak_count / area_m2),
            sd_per_m2 = sd(oak_count / area_m2), .groups = "drop")


#Change in total basal area between years, per site and treatment
oaks_noshoots %>% 
  group_by(site, treatment, year) %>% 
  summarise(mean_total_ba = mean(total_ba, na.rm = TRUE),
            mean_quercus_ba = mean(quercus_sp_ba, na.rm = TRUE), 
            .groups = "drop")

ba_table <- oaks_noshoots %>%
  group_by(site, treatment, year) %>%
  filter(year != 2005) %>% 
  summarise(mean_total_ba = mean(total_ba, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(total_ba = sprintf("%.1f", mean_total_ba)) %>%
  select(site, treatment, year, total_ba) %>%
  pivot_wider(names_from = year,
              values_from = total_ba,
              names_glue = "{year} basal area") %>%
  rename(Site = site,
         Treatment = treatment)

(ba_flextable <- ba_table %>%
  flextable() %>%
  merge_v(j = "Site") %>%
  theme_booktabs() %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all"))

save_as_docx(ba_flextable, path = "ba_table.docx")


#Overall BA change by treatment
oaks_noshoots %>%
  group_by(year, treatment) %>%
  summarise(mean_total_ba = mean(total_ba, na.rm = TRUE),
            mean_quercus_ba = mean(quercus_sp_ba, na.rm = TRUE))


#Change in canopy openness between periods, by site and treatment
oaks_period %>%
  group_by(site, treatment, period) %>%
  summarise(mean_canopy_openness = mean(canopy_openness, na.rm = TRUE),
            sd_canopy_openness = sd(canopy_openness, na.rm = TRUE), 
            .groups = "drop")

#Nicer table
canopy_table <- oaks_period %>%
  group_by(site, treatment, period) %>%
  summarise(mean_canopy_openness = mean(canopy_openness, na.rm = TRUE),
            sd_canopy_openness   = sd(canopy_openness, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(canopy = sprintf("%.1f ± %.1f",
                          mean_canopy_openness,
                          sd_canopy_openness)) %>%
  select(site, treatment, period, canopy) %>%
  pivot_wider(names_from = period,
              values_from = canopy,
              names_glue = "{period} canopy openness (%)") %>%
  rename(Site = site,
         Treatment = treatment)

(canopy_flextable <- canopy_table %>%
    flextable() %>%
    merge_v(j = "Site") %>%
    theme_booktabs() %>%
    autofit() %>%
    font(fontname = "Times New Roman", part = "all"))

save_as_docx(canopy_flextable, path = "canopy_openness_table.docx")


#Overall canopy openness change
oaks_period %>%
  group_by(period, treatment) %>%
  summarise(mean_canopy_openness = mean(canopy_openness, na.rm = TRUE),
            sd_canopy_openness = sd(canopy_openness, na.rm = TRUE))


#Mean oak seedling height, by site and treatment (note only 2025)
height_data %>%
  filter(species != "0", !is.na(height_cm), height_cm != ">130") %>%
  mutate(height_cm = as.numeric(height_cm),
         height_cm = ifelse(height_cm > 130, NA, height_cm)) %>%
  filter(!is.na(height_cm), shoot == FALSE) %>%
  group_by(site, treatment) %>%
  summarise(mean_oak_height = mean(height_cm[species == "Quercus sp."], na.rm = TRUE))


