#Data cleaning

install.packages("janitor")

library(readr)
library(dplyr)
library(janitor)

#Load datasets
seedling_raw <- read_csv2("raw_data/seedling_data_raw.csv")
old_data_raw <- read_csv2("raw_data/old_data_raw.csv")
canopy_raw <- read_csv2("raw_data/canopy_data_raw.csv")
tree_raw <- read_csv2("raw_data/tree_data_raw.csv")
soil_raw <- read_csv2("raw_data/soil_data_raw.csv")

#Remove the two last empty rows of soil_raw
soil_raw <- soil_raw %>%
  slice(1:(n() - 2))

#Remove row 38 with duplicate sample
soil_raw <- soil_raw %>%
  slice(-38)


#Show seedling density per subplot and species instead of heights
#Replace empty subplots (species = "0" rows) with Quercus sp., density = 0
#Count real seedlings
seedling_counts <- seedling_raw %>% 
  filter(species != "0") %>% 
  group_by(site, plot, treatment, transect, subplot, species) %>% 
  summarise(density = n(), .groups = "drop")

#Identify empty subplots
empty_subplots <- seedling_raw %>% 
  filter(species == "0") %>% 
  distinct(site, plot, treatment, transect, subplot)

#Create zero-density Quercus rows for empty subplots
empty_quercus <- empty_subplots %>% 
  mutate(species = "Quercus sp.",
         density = 0)

#Combine with real data
seedling_density <- bind_rows(seedling_counts, empty_quercus) %>% 
  arrange(site, plot, transect, subplot, species)


#Add canopy openness values
#Calculate mean canopy openness per subplot
canopy_mean <- canopy_raw %>% 
  group_by(Site, Plot, Treatment, Transect, Subplot) %>% 
  summarise(mean_canopy_openness = mean(`Canopy openness (%)`, na.rm = TRUE, .groups = "drop"))

#Add the means to seedling_density -> regeneration_data
regeneration_data <- seedling_density %>% 
  left_join(canopy_mean,
            by = c("site" = "Site",
                   "plot" = "Plot",
                   "treatment" = "Treatment",
                   "transect" = "Transect",
                   "subplot" = "Subplot")) %>% 
  select(site, plot, treatment, transect, subplot, mean_canopy_openness, everything())


#Add pH values to regeneration_data
regeneration_data <- regeneration_data %>% 
  left_join(soil_raw %>% 
              select(site, plot, treatment, transect, subplot, pH),
            by = c("site", "plot", "treatment", "transect", "subplot")) %>% 
  select(site, plot, treatment, transect, subplot, mean_canopy_openness, pH, everything())


#Add tree basal area to regeneration_data
#Calculate basal area for every individual tree
tree_ba <- tree_raw %>% 
  mutate(basal_area = pi * (diameter_cm /100 / 2)^2)

#Calculate basal area by species
species_ba <- tree_ba %>% 
  group_by(site, plot, transect_range, treatment, species) %>% 
  summarise(basal_area = sum(basal_area), .groups = "drop")

#Calculate total basal area
total_ba <- tree_ba %>% 
  group_by(site, plot, transect_range, treatment) %>% 
  summarise(total_ba = sum(basal_area), .groups = "drop")

#Convert to wide and clean names
species_ba_wide <- species_ba %>% 
  pivot_wider(names_from = species,
              values_from = basal_area,
              values_fill = 0) %>% 
  janitor::clean_names() %>% 
  rename_with(~ paste0(.x, "_ba"),
              -c(site, plot, transect_range, treatment)) %>% 
  select(site, plot, transect_range, treatment, quercus_sp_ba, everything())

#Add species and stand BA to regeneration_data
#Note: BA was only measured around one of the transects in each plot and BA values are therefore only added for these transects
regeneration_data <- regeneration_data %>% 
  left_join(species_ba_wide %>% select(-transect_range), 
            by = c("site", "plot", "treatment")) %>% 
  left_join(total_ba %>% select(-transect_range),
            by = c("site", "plot", "treatment")) %>%
  mutate(across(
    ends_with("_ba"), 
    ~ case_when(
      site == "Karla" & transect %in% c("NV45", "SO55") ~ .,
      site == "Rya åsar" & transect %in% c("NO80", "SV55") ~ .,
      site == "Sandviksås" & transect %in% c("V55", "Ö45") ~ .,
      site == "Skölvene" & transect %in% c("V55", "Ö55") ~ .,
      site == "Östadkulle" & transect %in% c("V75", "Ö75") ~ .,
      TRUE ~ NA_real_)))
  





