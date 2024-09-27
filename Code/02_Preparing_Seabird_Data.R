#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)
# code adapted from Casey Benkwitt

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "tidyverse", "ggplot2", 
#                   "readxl", "dplyr", "raster", "sp", "sf", "rgdal",
#                   "ggsn", "mapview", "viridis", "RColorBrewer", "here"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "readxl", "dplyr",
          "raster", "sp", "sf", "rgdal","ggsn", "mapview", "viridis",
          "RColorBrewer", "here")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch1/")
# set_here("E:/Data/StuartC_DPhil_Ch1/") set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

#### 2021 BASELINE DATA ####
# read in the baseline seabird data from 2021
sb_21 = read_xlsx(here("Data", "Seabirds",
                       "Tetiaroa_Society_Seabird_Transects_October_November_2021.xlsx"),
                  sheet = "Oct21TransectData",
                  col_names = TRUE,
                  col_types = c("guess", "numeric", "text", "text",
                                "date", "guess", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "numeric", "text", "text", 
                                "numeric", "text", "text", "guess",
                                "text", "text", "text", "text",
                                "text", "text"))
# seabird biomass conversions by species - has average adult mass per species 
# from Handbook of the Birds of the World (typical source, but should double-check 
# A. minutus and O. lunatus) and from Elton Traits paper 
# (https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/13-1917.1)
bio_dat = read.csv(here("Data", "Seabirds", "Tetiaroa_Seabird_Biomass_Conversions.csv"),
                   stringsAsFactors = TRUE)

# preview the datasets
str(sb_21)
str(bio_dat)

# remove observations from the Aie sandbar, which we're not interested in.
# according to the GPScoordinates tab, the sandbar transects are 1549, 
# 1550, 1551, 1718, 0952. 
# from map, they are: 1718-0952, 0952-1549, 1549-1550, 1550-1551, 1551-0945.
# from datasheet, only found: 0952-1548, 1548-1549, 1550-1551, 1551-1552. 
# Note for 1551-1552 says end transect also called 0952. 
sb_21 = sb_21 %>%
  mutate(TransectStart = as.factor(TransectStart)) 

sb_21 = sb_21 %>%
  filter(!(TransectStart == "1548"|TransectStart == "1550"|
             TransectStart == "1551"|TransectStart == "0952"))%>%
  droplevels()

length(unique(sb_21$TransectStart))  #242 total transects

# how many transects per motu?
sb_21_trans_nbr = sb_21 %>%
  group_by(Motu)%>%
  distinct(TransectStart, TransectEnd) %>%
  summarize(number_transects = length(TransectStart))%>%
  ungroup()

sb_21_trans_nbr

# add a new column to data to store the number of transects for reference
sb_21 = left_join(sb_21, sb_21_trans_nbr, by = "Motu")%>%
  relocate(number_transects, .after= Motu)
sb_21

# now add in the seabird biomass info
sb_21_bio = left_join(sb_21, select(bio_dat, c(SpeciesName, Body_mass_HBW)),
            by = "SpeciesName")%>%
  relocate(Body_mass_HBW, .after = SpeciesName)

NAs = sb_21_bio %>%
  filter(is.na(Body_mass_HBW))
# looks good - only NA's are for "NO BIRDS" or unidentified bird
rm(NAs)

# exclude seaward birds and limit data to "within 3" or "3 to 5"
# Casey update: also keep 0 and what about blank - these seem to be flushed (at least for Iti - see below)*****
sb_21_bio = sb_21_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(sb_21_bio$Distance.m) # how many different distances were recorded?

# filter to desired levels
sb_21_bio = sb_21_bio %>%
  filter(Distance.m == "within 3" | Distance.m == "3 to 5" |
           Distance.m == "0"| Distance.m == "")%>%
  droplevels()

# now count all nests by species
# this will give the total number of apparently occupied nests (AON), which 
# is standard measurement for seabirds and from this we can estimate the number 
# (and biomass, etc) of breeding pairs.
sb_21_bio = sb_21_bio %>%
  mutate(StageCategory = as.factor(StageCategory))

levels(sb_21_bio$StageCategory) # levels of StageCategory

# Notes from Casey and Jayna:

# NOTE: "juv" category shoudn't be counted here because "Since the “juveniles” 
# are fully mobile adult-like birds they aren’t associated with nests. - Jayna"

# NOTE: currently assuming each egg/chick/fledgling is in its OWN nest - this
# is correct - "all eggs, chicks, and fledglings are on their own nest. the 
# only species that lays more than one egg is the brown booby, but there
# are no brown booby nests on those motu. I believe did try to make only
# one line per nest even for brown boobies, but for those there can be more 
# than one egg or chick in the same nest. - Jayna"

# limit to nests
sb_21_nests = sb_21_bio %>%
  filter(StageCategory == "chick" | StageCategory == "egg"| 
           StageCategory == "fledgling"| StageCategory == "nest"|
           StageCategory == "onegg")%>%
  droplevels()

# now add by species/motu and calculate density
sb_21_nests_motu_sp = sb_21_nests %>%
  group_by(Motu, number_transects, SpeciesName, Body_mass_HBW)%>%
  summarize(BreedingPairs = sum(Nb),
            Abundance = BreedingPairs*2)%>% # multiply by 2 - assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance/(500*number_transects)) %>% # 5 x 100 m transects, so each transect = 500 m, times the number of transects
  mutate(Density_ha = Density_m2*10000) %>% # and convert to hectares because that's used a lot
  mutate(biomass_kg = Abundance*Body_mass_HBW/1000, # calculate biomass per species  - divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg/(500*number_transects), # convert to per m^2 as above
         biomass_kg_ha = biomass_kg_m2 *10000) # convert to per hectare - divide by 500 because 500 m^2 per transect, multiple by 10000 for m^2 to ha

# now sum for total biomass (rather than by species), combine all scales into one table
# nesting birds
sb_21_nests_motu = sb_21_nests_motu_sp %>%
  group_by(Motu)%>%
  summarize(breeding_density_ha_motu = sum(Density_ha),
            breeding_biomass_kgha_motu = sum(biomass_kg_ha))
sb_21_nests_motu

#### 2023 DATA ####
# updated seabird data from 2023
sb_23 = read_xlsx(here("Data", "Seabirds",
                       "Tetiaroa_Society_Seabird_Transects_August_2023.xlsx"),
                  sheet = "TransectData",
                  col_names = TRUE,
                  col_types = c("guess", "numeric", "text", "text",
                                "date", "guess", "text", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "numeric", "numeric", "numeric",
                                "numeric", "text", "text", "text", "numeric", 
                                "text", "text", "guess", "text", "text", 
                                "text", "text", "text", "text", "text", "text"))

str(sb_23)

# remove observations from the Aie sandbar, which we're not interested in.
# according to the GPScoordinates tab, the sandbar transects are 
# "1718to0952_Sandbar", "0952to1549_Sandbar", "1549to1550_Sandbar",
# "1550to1551_Sandbar", "1551to0945_Sandbar"
sb_23 = sb_23 %>%
  mutate(TransectName = as.factor(TransectName)) 

sb_23 = sb_23 %>%
  filter(!(TransectStart == "1549"|TransectStart == "1550"|
             TransectStart == "1551"|TransectStart == "0952"))%>%
  droplevels()

length(unique(sb_23$TransectStart))  #319 total transects

# how many transects per motu?
sb_23_trans_nbr = sb_23 %>%
  group_by(Motu)%>%
  distinct(TransectStart, TransectEnd) %>%
  summarize(number_transects = length(TransectStart))%>%
  ungroup()

sb_23_trans_nbr

# add a new column to data to store the number of transects for reference
sb_23 = left_join(sb_23, sb_23_trans_nbr, by = "Motu")%>%
  relocate(number_transects, .after= Motu)
sb_23

# now add in the biomass info

# the 2023 transects also had Masked booby (Sula dactylatra), which were not 
# seen in 2021 nor included in the original biomass data. add a new row for
# Sdactylatra using biomass estimates in the BirdFuncDat.txt file of 
# EltonTraits 1.0: Species-level foraging attributes of the world's birds
# and mammals (available at https://figshare.com/articles/dataset/Data_Paper_Data_Paper/3559887?backTo=/collections/EltonTraits_1_0_Species-level_foraging_attributes_of_the_world_s_birds_and_mammals/3306933)
masked_booby = data.frame(
  Scientific_name = "Sula.dactylatra",
  SpeciesName = "Sdactylatra",
  Common_name = "Masked booby",
  Body_mass_HBW = 1732.08,
  HBW.mass.notes = "from EltonTraits 1.0, source Dunning08",
  stringsAsFactors = FALSE)

# add this to the bio_dat from 2021
bio_dat_23 = rbind(select(bio_dat, Scientific_name, SpeciesName,
                          Common_name, Body_mass_HBW, HBW.mass.notes), 
                   masked_booby)

sb_23_bio = left_join(sb_23, select(bio_dat_23, SpeciesName, Body_mass_HBW), 
                      by = c("Species" = "SpeciesName")) %>%
  relocate(Body_mass_HBW, .after = Species)

NAs = sb_23_bio %>%
  filter(is.na(Body_mass_HBW))
# looks good - only NA's are for NOBIRDS, crabs, shorebirds, fish, etc.
rm(NAs)

# exclude seaward birds and limit data to "within 3" or "3 to 5"
sb_23_bio = sb_23_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(sb_23_bio$Distance.m) # how many different distances were recorded?

# filter to desired levels
sb_23_bio = sb_23_bio %>%
  filter(Distance.m == "within 3" | Distance.m == "3 to 5" |
           Distance.m == "0"| Distance.m == "") %>%
  droplevels()

# now count all nests by species
# this will give the total number of apparently occupied nests (AON), which 
# is standard measurement for seabirds and from this we can estimate the number 
# (and biomass, etc) of breeding pairs.
sb_23_bio = sb_23_bio %>%
  mutate(StageCategory = as.factor(StageCategory))

levels(sb_23_bio$StageCategory) # levels of StageCategory

# limit to nests
sb_23_nests = sb_23_bio %>%
  filter(StageCategory == "chick" | StageCategory == "egg"| 
           StageCategory == "eggs" | StageCategory == "fledgling"| 
           StageCategory == "nest"| StageCategory == "onegg" | 
           StageCategory == "oneggs")%>%
  droplevels()

# now add by species/motu and calculate density
sb_23_nests_motu_sp = sb_23_nests %>%
  group_by(Motu, number_transects, SpeciesName, Species, Body_mass_HBW)%>%
  summarize(BreedingPairs = sum(Nb),
            Abundance = BreedingPairs*2)%>% # multiply by 2 assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance/(500*number_transects)) %>% # 5 x 100 m transects, so each transect = 500 m, times the number of transects
  mutate(Density_ha = Density_m2*10000) %>% # and convert to hectares because that's used a lot
  mutate(biomass_kg = Abundance*Body_mass_HBW/1000, # calculate biomass per species  - divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg/(500*number_transects), # convert to per m^2 as above
         biomass_kg_ha = biomass_kg_m2 *10000) # convert to per hectare - divide by 500 because 500 m^2 per transect, multiple by 10000 for m^2 to ha

# now sum for total biomass (rather than by species), combine all scales into one table
# nesting birds
sb_23_nests_motu = sb_23_nests_motu_sp %>%
  group_by(Motu)%>%
  summarize(breeding_density_ha_motu = sum(Density_ha),
            breeding_biomass_kgha_motu = sum(biomass_kg_ha))
sb_23_nests_motu

#### AVERAGE 2021 & 2023 DATA ####
# bring together the 2021 and 2023 seabird data
seabirds = rbind(mutate(sb_21_nests_motu, Year = 2021),
                 mutate(sb_23_nests_motu, Year = 2023))
# calculate the mean and standard deviation for breeding seabird biomass and density
seabirds_avg = seabirds %>%
  group_by(Motu) %>%
  summarise(avg_breeding_density_ha_motu = mean(breeding_density_ha_motu),
            std_breeding_density_ha_motu = sd(breeding_density_ha_motu),
            avg_breeding_biomass_kgha_motu = mean(breeding_biomass_kgha_motu),
            std_breeding_biomass_kgha_motu = sd(breeding_biomass_kgha_motu))
# no standard deviation for Honuea because this motu was only sampled in 2021

# save the prepped breeding seabird data
write.csv(seabirds_avg,
          here("Data", "Seabirds", "Seabird_Summary_Data_2021_2023.csv"),
          row.names = FALSE)