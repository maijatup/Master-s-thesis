#Data cleaning

library(readr)
library(dplyr)

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

#Add the means to seedling_density
seedling_density <- seedling_density %>% 
  left_join(canopy_mean,
            by = c("site" = "Site",
                   "plot" = "Plot",
                   "treatment" = "Treatment",
                   "transect" = "Transect",
                   "subplot" = "Subplot")) %>% 
  select(site, plot, treatment, transect, subplot, mean_canopy_openness, everything())



