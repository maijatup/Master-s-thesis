#Exploratory figures 16/3/2026

library(readr)
library(dplyr)
library(ggplot2)

regeneration_all <- read_csv("processed_data/regeneration_all.csv")


#Oak density 2003 vs 2005 vs 2025
oak_density <- regeneration_all %>%
  filter(species == "Quercus sp.") %>%
  group_by(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, quercus_sp_ba, total_ba) %>%
  summarise(oak_density = sum(density), .groups = "drop")

ggplot(oak_density,
       aes(x = factor(year), y = oak_density)) +
  geom_boxplot() + 
  labs(x = "Year", y = "Oak density")

#Control and treatment separately
ggplot(oak_density,
       aes(x = factor(year), y = oak_density, fill = treatment)) +
  geom_boxplot() + 
  labs(x = "Year", y = "Oak density")

#Overall oak density by treatment (not important)
ggplot(oak_density,
       aes(x = factor(treatment), y = oak_density)) +
  geom_boxplot() + 
  labs(x = "treatment", y = "Oak density")


#Total seedling density 2003 vs 2005 vs 2025
total_density <- regeneration_all %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, total_ba) %>% 
  summarise(total_density = sum(density), .groups = "drop")

ggplot(total_density,
       aes(x = factor(year), y = total_density)) +
  geom_boxplot() + 
  labs(x = "Year", y = "Total seedling density")

#Control and treatment separately
ggplot(total_density,
       aes(x = factor(year), y = total_density, fill = treatment)) +
  geom_boxplot() + 
  labs(x = "Year", y = "Total seedling density")

#Total density by treatment (not important)
ggplot(total_density,
       aes(x = factor(treatment), y = total_density)) +
  geom_boxplot() + 
  labs(x = "treatment", y = "Total density")


#Oak density vs canopy openness, 2005 & 2025 (2003 no canopy values)
ggplot(oak_density, aes(x = canopy_openness, y = oak_density)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2005 & 2025)")

#2005 & 2025 with treatment
ggplot(oak_density, aes(x = canopy_openness, y = oak_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2005 & 2025)")

#2005
ggplot(oak_density %>% filter(year == 2005), 
       aes(x = canopy_openness, y = oak_density)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2005)")

#2005 with treatment
ggplot(oak_density %>% filter(year == 2005), 
       aes(x = canopy_openness, y = oak_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2005)")

#2025 (canopy openness was measured for all subplots)
ggplot(oak_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = oak_density)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2025)")

#2025 with treatment
ggplot(oak_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = oak_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Oak seedling density",
       title = "Oak density vs canopy openness (2025)")


#Total seedling density vs canopy openness, 2005 & 2025
ggplot(total_density, aes(x = canopy_openness, y = total_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2005 & 2025)")

#2005 & 2025 with treatment
ggplot(total_density, aes(x = canopy_openness, y = total_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2005 & 2025)")

#2005
ggplot(total_density %>% filter(year == 2005),
       aes(x = canopy_openness, y = total_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2005)")

#2005 with treatment
ggplot(total_density %>% filter(year == 2005),
       aes(x = canopy_openness, y = total_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2005)")

#2025
ggplot(total_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = total_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2025)")

#2025 with treatment
ggplot(total_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = total_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density",
       title = "Total seedling density vs canopy openness (2025)")

#Without oak
others_density <- regeneration_all %>% 
  filter(species != "Quercus sp.") %>% 
  group_by(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, quercus_sp_ba, total_ba) %>%
  summarise(others_density = sum(density), .groups = "drop")

#2005 & 2025 without oak
ggplot(others_density, aes(x = canopy_openness, y = others_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oaks) vs canopy openness (2005 & 2025)")

#With treatment
ggplot(others_density, aes(x = canopy_openness, y = others_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oaks) vs canopy openness (2005 & 2025)")

#2005 without oak
ggplot(others_density %>% filter(year == 2005),
       aes(x = canopy_openness, y = others_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oaks) vs canopy openness (2005)")

#With treatment
ggplot(others_density %>% filter(year == 2005),
       aes(x = canopy_openness, y = others_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oaks) vs canopy openness (2005)")

#Only 2025 without oak
ggplot(others_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = others_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oak) vs canopy openness (2025)")

#With treatment
ggplot(others_density %>% filter(year == 2025),
       aes(x = canopy_openness, y = others_density, fill = treatment, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Canopy openness (%)",
       y = "Total seedling density without oak",
       title = "Total seedling density (no oak) vs canopy openness (2025)")


#Oak density vs pH
ggplot(oak_density, aes(x = pH, y = oak_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "pH", y = "Oak seedling density")

#Total seedling density vs pH
ggplot(total_density, aes(x = pH, y = total_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "pH", y = "Total seedling density")

#Other species' seedling density vs pH
ggplot(others_density, aes(x = pH, y = others_density, fill = treatment, color = treatment)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "pH", y = "Other species' seedling density")


#Oak seedling density vs oak basal area (color = site?)
ggplot(oak_density, aes(x = quercus_sp_ba, y = oak_density, fill = treatment)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Oak basal area", y = "Oak seedling density")

#Oak seedling density vs total basal area
ggplot(oak_density, aes(x = total_ba, y = oak_density, fill = treatment)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Total basal area", y = "Oak seedling density")


