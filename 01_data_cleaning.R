#Data cleaning

install.packages("janitor")

library(readr)
library(dplyr)
library(janitor)

#Load datasets
seedling_raw <- read_csv2("raw_data/seedling_data_raw.csv")
old_data_raw <- read.csv("raw_data/old_data_raw.csv", sep = ";", dec = ".")
canopy_raw <- read_csv2("raw_data/canopy_data_raw.csv")
tree_raw <- read_csv2("raw_data/tree_data_raw.csv")
soil_raw <- read_csv2("raw_data/soil_data_raw.csv")

#Remove the two last empty rows of soil_raw
soil_raw <- soil_raw %>%
  slice(1:(n() - 2))

#Remove row 38 with duplicate sample
soil_raw <- soil_raw %>%
  slice(-38)

#Add year and subplot area to seedling data
#Add a shoot column
seedling_raw <- seedling_raw %>% 
  mutate(year = 2025,
         area_m2 = 5,
         shoot = notes %in% c(
           "stubbskott, räknas ej?",
           "stubbskott",
           "stubbskott på ovan rönn",
           "stubbskott av nian")) %>% 
  select(site, plot, treatment, year, transect, subplot, position_m, area_m2, everything())

#Dataset including trunk shoots
seedling_raw_all <- seedling_raw

#Dataset excluding trunk shoots
seedling_raw_no_shoots <- seedling_raw %>% 
  filter(!shoot == TRUE)


#Function to calculate seedling density per subplot and species
calculate_seedling_density <- function(data) {
  
  #Replace empty subplots with Quercus sp., density = 0
  #Count real seedlings
  seedling_counts <- data %>% 
    filter(species != "0") %>% 
    group_by(site, plot, treatment, year, transect, subplot, area_m2, species) %>% 
    summarise(density = n(), .groups = "drop")
  
  #Identify empty subplots
  empty_subplots <- data %>% 
    filter(species == "0") %>% 
    distinct(site, plot, treatment, year, transect, subplot, area_m2)
  
  #Create zero-density Quercus rows for empty subplots
  empty_quercus <- empty_subplots %>% 
    mutate(species = "Quercus sp.",
           density = 0)
  
  #Combine with real data
  seedling_density <- bind_rows(seedling_counts, empty_quercus)
  
  #Identify subplots without Quercus and create zero-density rows
  missing_quercus <- seedling_density %>% 
    group_by(site, plot, treatment, year, transect, subplot, area_m2) %>% 
    filter(!any(species == "Quercus sp.")) %>% 
    distinct(site, plot, treatment, year, transect, subplot, area_m2) %>% 
    mutate(species = "Quercus sp.",
           density = 0)
  
  #Combine everything
  seedling_density <- bind_rows(seedling_density, missing_quercus) %>% arrange(site, plot, treatment, year, transect, subplot, area_m2, species)
  
  return(seedling_density)
}

#Seedling density including trunk shoots
seedling_density_all <- calculate_seedling_density(seedling_raw_all)

#Seedling density excluding trunk shoots
seedling_density_no_shoots <- calculate_seedling_density(seedling_raw_no_shoots)


#Add canopy openness values
#Calculate mean canopy openness per subplot
canopy_mean <- canopy_raw %>% 
  group_by(Site, Plot, Treatment, Transect, Subplot) %>% 
  summarise(canopy_openness = mean(`Canopy openness (%)`, na.rm = TRUE, .groups = "drop"))

#Add the means to seedling_density datasets
#Including trunk shoots
data_2025_all <- seedling_density_all %>% 
  left_join(canopy_mean,
            by = c("site" = "Site",
                   "plot" = "Plot",
                   "treatment" = "Treatment",
                   "transect" = "Transect",
                   "subplot" = "Subplot")) %>% 
  select(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, everything())

#Excluding trunk shoots
data_2025_no_shoots <- seedling_density_no_shoots %>% 
  left_join(canopy_mean,
            by = c("site" = "Site",
                   "plot" = "Plot",
                   "treatment" = "Treatment",
                   "transect" = "Transect",
                   "subplot" = "Subplot")) %>% 
  select(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, everything())


#Add pH values to both datasets
#Including trunk shoots
data_2025_all <- data_2025_all %>% 
  left_join(soil_raw %>% 
              select(site, plot, treatment, transect, subplot, pH),
            by = c("site", "plot", "treatment", "transect", "subplot")) %>% 
  select(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, everything())

#Excluding trunk shoots
data_2025_no_shoots <- data_2025_no_shoots %>% 
  left_join(soil_raw %>% 
              select(site, plot, treatment, transect, subplot, pH),
            by = c("site", "plot", "treatment", "transect", "subplot")) %>% 
  select(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, everything())


#Add tree basal area to both datasets
#Calculate basal area for every individual tree
tree_ba <- tree_raw %>% 
  mutate(basal_area = pi * (diameter_cm/100/2)^2)

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

#Add species and stand BA to both datasets
#Note: BA was only measured around one of the transects in each plot and BA values are therefore only added for these transects
#Including trunk shoots
data_2025_all <- data_2025_all %>% 
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

#Excluding trunk shoots
data_2025_no_shoots <- data_2025_no_shoots %>% 
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




#Clean old data
#Change date to year
#Change empty diameter cells to NA
old_data_raw <- old_data_raw %>% 
  mutate(year = as.integer(format(as.Date(date), "%Y"))) %>% 
  mutate(diameter_cm = na_if(diameter_cm, "")) %>% 
  select(site, plot, treatment, year, (everything()))

#Remove data from 2001 (before thinning) and 2002 (only recorded seedlings <20 cm)
#Remove the extra subplot from 2003 with unknown size
#Remove subplots that were within exclosures
#Remove raspberry Rubus idaeus since they were not always recorded
#Remove trees larger than 5 cm DBH
#Remove subplots that were not checked in 2003 and 2005 so that these zeros don't skew the dataset
old_data_03_05 <- old_data_raw %>% 
  filter(!year %in% c(2001, 2002),
         subplot != "extra",
         species != "Rubus idaeus",
         in_exclosure != TRUE) %>% 
  filter(is.na(diameter_cm) |
           suppressWarnings(as.numeric(diameter_cm)) <= 5 |
           diameter_cm == "0.1-5") %>% 
  filter(!(site == "Skölvene" & transect == "Ö35" & subplot == 3),
         !(site == "Karla" & transect == "NV45" & subplot %in% c(2, 3, 4)))


#Dataset including all seedlings
data_03_05_all <- old_data_03_05

#Dataset excluding trunk shoots
data_03_05_no_shoots <- old_data_03_05 %>% 
  filter(!shoot == TRUE)


#Function to calculate seedling density per subplot and species
calculate_seedling_density_old <- function(data) {
  
  #Replace empty subplots with Quercus sp., density = 0
  #Count real seedlings
  seedling_counts <- data %>% 
    filter(species != "0") %>% 
    group_by(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, species) %>% 
    summarise(density = n(), .groups = "drop")
  
  #Identify empty subplots
  empty_subplots <- data %>% 
    filter(species == "0") %>% 
    distinct(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH)
  
  #Create zero-density Quercus rows for empty subplots
  empty_quercus <- empty_subplots %>% 
    mutate(species = "Quercus sp.",
           density = 0)
  
  #Combine with real data
  seedling_density <- bind_rows(seedling_counts, empty_quercus)
  
  #Identify subplots without Quercus and create zero-density rows
  missing_quercus <- seedling_density %>% 
    group_by(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH) %>% 
    filter(!any(species == "Quercus sp.")) %>% 
    distinct(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH) %>% 
    mutate(species = "Quercus sp.",
           density = 0)
  
  #Combine everything
  seedling_density <- bind_rows(seedling_density, missing_quercus) %>% arrange(site, plot, treatment, year, transect, subplot, area_m2, canopy_openness, pH, species)
  
  return(seedling_density)
}

#Complete old dataset including trunk shoots
data_03_05_all <- calculate_seedling_density_old(data_03_05_all)

#Complete old dataset excluding trunk shoots
data_03_05_no_shoots <- calculate_seedling_density_old(data_03_05_no_shoots)


#Combine the old and new datasets and save as csv files
regeneration_all <- bind_rows(data_03_05_all, data_2025_all)
write_csv(regeneration_all, "processed_data/regeneration_all.csv")

regeneration_no_shoots <- bind_rows(data_03_05_no_shoots, data_2025_no_shoots)
write_csv(regeneration_no_shoots, "processed_data/regeneration_no_shoots.csv")



