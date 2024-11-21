#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)
# code adapted from Casey Benkwitt

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("conflicted", "tidyverse", "ggplot2", "readxl", "dplyr",
#                    "raster", "sp", "sf", "rgdal", "ggsn", "mapview", "viridis",
#                    "RColorBrewer", "here", "terra", "gstat"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "readxl", "dplyr",
          "raster", "sp", "sf", "rgdal", "ggsn", "mapview", "viridis",
          "RColorBrewer", "here", "terra", "gstat")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
rasterOptions(tmpdir = "F:/Data/StuartC_DPhil_Ch1/Temp/") # custom directory for temporary files
terraOptions(tempdir = "F:/Data/StuartC_DPhil_Ch1/Temp/") # custom directory for temporary files
rasterOptions(progress = "text") # print raster processing progress
terraOptions(progress = 1) # print raster processing progress

#### DIRECTORIES ####
# working directory and relative folder path
setwd("F:/Data/StuartC_DPhil_Ch1/")
# set_here("F:/Data/StuartC_DPhil_Ch1/") set first-time only
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

# read in the associated gps coordinates
gps_21 = read_xlsx(here("Data", "Seabirds",
                        "Tetiaroa_Society_Seabird_Transects_October_November_2021.xlsx"),
                   sheet = "GPScoordinates",
                   col_names = TRUE,
                   col_types = c("text", "numeric", "numeric", "numeric",
                                 "text", "text"))

# add columns for the starting and ending coordinates of each transect
sb_21 = sb_21 %>%
  left_join(gps_21 %>% select(Name, Latitude, Longitude), 
            by = c("TransectStart" = "Name"), relationship = "many-to-many") %>%
  rename(Latitude_Start = Latitude,
         Longitude_Start = Longitude)

sb_21 = sb_21 %>%
  left_join(gps_21 %>% select(Name, Latitude, Longitude), 
            by = c("TransectEnd" = "Name"), relationship = "many-to-many") %>%
  rename(Latitude_End = Latitude,
         Longitude_End = Longitude)

# seabird biomass conversions by species - has average adult mass per species 
# from Handbook of the Birds of the World and from Elton Traits paper 
# (https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/13-1917.1)
bio_dat = read.csv(here("Data", "Seabirds", "Tetiaroa_Seabird_Biomass_Conversions.csv"),
                   stringsAsFactors = TRUE)

# preview the datasets
str(sb_21)
str(bio_dat)

# some Tahuna Iti transects that are only 50m long, separate these
iti_50m = sb_21 %>%
  filter(TransectStart %in% c("0761", "0764", "0768"))

# save the gps coordinates for these shorter transects for later
iti_50m_gps = iti_50m %>%
  select(Motu, TransectStart, TransectEnd, 
         Latitude_Start, Longitude_Start, 
         Latitude_End, Longitude_End)

# now keep only the 100m transects, but remove the inland ones from Tiaraunu, Rimatuu, and Reiono,
# and the Aie sandbar, which we're not interested in
sb_21 = sb_21 %>%
  filter(!TransectStart %in%
           c("1105", "1010", "1011", "1016", "1017", "1018", "0952", "1549", "1550", "1551", 
             "1718","0649", "0650", "0651", "0652", "0653", "0654", "0655", "0656", "0657",
             "1153", "1154", "1155", "1157", "1158", "1128", "1130", "1131", "1132", "1134", 
             "1185", "1186", "1187", "1188", "0761", "0764", "0768", "1004", "1005")) %>%
  droplevels()

length(unique(sb_21$TransectStart)) 

# now add in the seabird biomass info
sb_21_bio = left_join(sb_21, select(bio_dat, c(SpeciesName, Body_mass_HBW)),
                      by = "SpeciesName") %>%
  relocate(Body_mass_HBW, .after = SpeciesName)

# do the same for the shorter transects
iti_50m_bio = left_join(iti_50m, select(bio_dat, c(SpeciesName, Body_mass_HBW)),
                    by = "SpeciesName") %>%
  relocate(Body_mass_HBW, .after = SpeciesName)

# check for NAs
NAs = sb_21_bio %>%
  filter(is.na(Body_mass_HBW)) # only NA's are for "NO BIRDS" or unidentified bird
iti_50m_bio %>%
  filter(is.na(Body_mass_HBW)) # no NAs in the Tahuna Iti 50m transects
rm(NAs)

# save the no bird records 
no_bird_sites = sb_21_bio %>%
  filter(Species == "NOBIRDS")

# exclude seaward birds and limit data to "within 3" or "3 to 5"
# Casey update: also keep 0 and what about blank - these seem to be flushed (at least for Iti - see below)*****
sb_21_bio = sb_21_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(sb_21_bio$Distance.m) # how many different distances were recorded?

iti_50m_bio = iti_50m_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(iti_50m_bio$Distance.m) # how many different distances were recorded?

# filter to desired levels
sb_21_bio = sb_21_bio %>%
  filter(Distance.m == "within 3" | Distance.m == "3 to 5" |
           Distance.m == "0"| Distance.m == "")%>%
  droplevels()

iti_50m_bio = iti_50m_bio %>%
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

iti_50m_bio = iti_50m_bio %>%
  mutate(StageCategory = as.factor(StageCategory))

levels(iti_50m_bio$StageCategory) # levels of StageCategory

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

iti_50m_nests = iti_50m_bio %>%
  filter(StageCategory == "chick" | StageCategory == "egg"| 
           StageCategory == "fledgling"| StageCategory == "nest"|
           StageCategory == "onegg")%>%
  droplevels()

# now calculate biomass and density per species per transect
sb_21_sp_transect = sb_21_nests %>%
  group_by(Motu, TransectStart, TransectEnd, SpeciesName, Body_mass_HBW) %>%
  summarize(BreedingPairs = sum(Nb), # number of breeding pairs
            Abundance = BreedingPairs * 2) %>%  # multiply by 2 - assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance / 500, # density per m^2, each transect is 5m x 100m (500m^2)
         Density_ha = Density_m2 * 10000,  # convert to density per hectare
         biomass_kg = Abundance * Body_mass_HBW / 1000, # biomass in kg, divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg / 500, # biomass per m^2
         biomass_kg_ha = biomass_kg_m2 * 10000)# biomass per hectare

iti_50m_sp_transect = iti_50m_nests %>%
  group_by(Motu, TransectStart, TransectEnd, SpeciesName, Body_mass_HBW) %>%
  summarize(BreedingPairs = sum(Nb), # number of breeding pairs
            Abundance = BreedingPairs * 2) %>%  # multiply by 2 - assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance / 250, # density per m^2, # each transect is 5m x 50m (250m^2)
         Density_ha = Density_m2 * 10000,  # convert to density per hectare
         biomass_kg = Abundance * Body_mass_HBW / 1000, # biomass in kg, divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg / 250, # biomass per m^2
         biomass_kg_ha = biomass_kg_m2 * 10000)# biomass per hectare

# now sum for total biomass at the transect level (rather than by species)
sb_21_transect = sb_21_sp_transect %>%
  group_by(Motu, TransectStart, TransectEnd)%>%
  summarize(breeding_density_ha_transect = sum(Density_ha),
            breeding_biomass_kgha_transect = sum(biomass_kg_ha))

# do the same for the shorter Iti transects
iti_50m_transect = iti_50m_sp_transect %>%
  group_by(Motu, TransectStart, TransectEnd)%>%
  summarize(breeding_density_ha_transect = sum(Density_ha),
            breeding_biomass_kgha_transect = sum(biomass_kg_ha))

# add back in the gps coordinates that we lost along the way
sb_21_transect = left_join(sb_21_transect, 
                           select(sb_21, TransectStart, TransectEnd, 
                                  Latitude_Start, Longitude_Start, 
                                  Latitude_End, Longitude_End)) %>%
  distinct() %>%
  drop_na(Latitude_Start, Longitude_Start, 
          Latitude_End, Longitude_End)

# add back in the gps coordinates of the shorter Iti transects that we saved above
iti_50m_transect = left_join(iti_50m_transect, 
                           select(iti_50m_gps, TransectStart, TransectEnd, 
                                  Latitude_Start, Longitude_Start, 
                                  Latitude_End, Longitude_End)) %>%
  distinct() %>%
  drop_na(Latitude_Start, Longitude_Start, 
          Latitude_End, Longitude_End)

# finally, add back in the sites with no birds at all
no_bird_sites = no_bird_sites %>%
  select(Motu, TransectStart, TransectEnd, Latitude_Start, Longitude_Start,
         Latitude_End, Longitude_End) %>%
  mutate(breeding_density_ha_transect = 0,
         breeding_biomass_kgha_transect = 0)

# bringing together ALL 2021 transect data
sb_21_transect = rbind(sb_21_transect, iti_50m_transect, no_bird_sites)

# converting from wide format to long format
sb_21_transect_long = sb_21_transect %>%
  # step 1: concatenate TransectStart and TransectEnd
  mutate(Transect = paste0(TransectStart, "to", TransectEnd)) %>%
  
  # step 2: pivot the Latitude and Longitude columns to long format
  pivot_longer(cols = c(Latitude_Start, Longitude_Start, Latitude_End, Longitude_End),
               names_to = c(".value","Coordinate_Type"),
               names_pattern = "(.*)_(Start|End)") %>%
  
  # step 3: rename columns as needed and rearrange them
  rename(Start_or_End = Coordinate_Type) %>%
  rename(Transect_Start = TransectStart) %>%
  rename(Transect_End = TransectEnd) %>%
  select(Motu, Transect, Transect_Start, Transect_End, 
         breeding_density_ha_transect, breeding_biomass_kgha_transect,
         Latitude, Longitude, Start_or_End) 

#### 2023 DATA ####
# repeat the process above for the 2023 seabird data
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

# keep only seabird records, remove observations of crabs, shorebirds, fish, etc.
sb_23 = sb_23 %>%
  filter(OrganismType == "Seabird")

#  read in the associated gps coordinates
gps_23 = read_xlsx(here("Data", "Seabirds",
                        "Tetiaroa_Society_Seabird_Transects_August_2023.xlsx"),
                   sheet = "GPSCoordinates_TransectLengths",
                   col_names = TRUE,
                   col_types = c("text", "text", "numeric", "numeric", 
                                 "numeric", "text", "text", "numeric",
                                 "text", "text", "text", "text", "text"))

# rename to match 2021 data above
gps_23 = gps_23 %>% rename(Name = StartName)

# add columns for the starting and ending coordinates of each transect
sb_23 = sb_23 %>%
  left_join(gps_23 %>% select(Name, Latitude, Longitude), 
            by = c("TransectStart" = "Name"),relationship = "many-to-many") %>%
  rename(Latitude_Start = Latitude,
         Longitude_Start = Longitude)

sb_23 = sb_23 %>%
  left_join(gps_23 %>% select(Name, Latitude, Longitude), 
            by = c("TransectEnd" = "Name"), relationship = "many-to-many") %>%
  rename(Latitude_End = Latitude,
         Longitude_End = Longitude)

# some Tahuna Iti transects that are only 50m long, separate these
iti_50m = sb_23 %>%
  filter(TransectStart %in% c("0761", "0764", "0768"))

# save these gps points for later
iti_50m_gps = iti_50m %>%
  select(Motu, TransectStart, TransectEnd, 
         Latitude_Start, Longitude_Start, 
         Latitude_End, Longitude_End)

# now keep only the 100m transects, but remove the inland ones from Tiaraunu, Rimatuu, and Reiono,
# and the Aie sandbar, which we're not interested in
sb_23 = sb_23 %>%
  filter(!TransectStart %in%
           c("1105", "1010", "1011", "1016", "1017", "1018", "0952", "1549", "1550", "1551", 
             "1718","0649", "0650", "0651", "0652", "0653", "0654", "0655", "0656", "0657",
             "1153", "1154", "1155", "1157", "1158", "1128", "1130", "1131", "1132", "1134", 
             "1185", "1186", "1187", "1188", "0761", "0764", "0768", "1004", "1005")) %>%
  droplevels()

length(unique(sb_23$TransectStart))

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

# now add in the seabird biomass info
sb_23_bio = left_join(sb_23, select(bio_dat_23, SpeciesName, Body_mass_HBW), 
                      by = c("Species" = "SpeciesName")) %>%
  relocate(Body_mass_HBW, .after = Species)

iti_50m_bio = left_join(iti_50m, select(bio_dat_23, c(Common_name, Body_mass_HBW)),
                        by = c("SpeciesName" = "Common_name")) %>%
  relocate(Body_mass_HBW, .after = SpeciesName)

# check for any NAs
count(sb_23_bio %>%
  filter(is.na(Body_mass_HBW))) 
count(iti_50m_bio %>%
  filter(is.na(Body_mass_HBW)))

# exclude seaward birds and limit data to "within 3" or "3 to 5"
sb_23_bio = sb_23_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(sb_23_bio$Distance.m) # how many different distances were recorded?

iti_50m_bio = iti_50m_bio %>%
  rename(Distance.m = `Distance-m`) %>% # rename for convenience
  mutate(Distance.m = as.factor(Distance.m))
levels(iti_50m_bio$Distance.m) # how many different distances were recorded?

# filter to desired levels
sb_23_bio = sb_23_bio %>%
  filter(Distance.m == "within 3" | Distance.m == "3 to 5" |
           Distance.m == "0"| Distance.m == "")%>%
  droplevels()

iti_50m_bio = iti_50m_bio %>%
  filter(Distance.m == "within 3" | Distance.m == "3 to 5" |
           Distance.m == "0"| Distance.m == "")%>%
  droplevels()

# now count all nests by species
# this will give the total number of apparently occupied nests (AON), which 
# is standard measurement for seabirds and from this we can estimate the number 
# (and biomass, etc) of breeding pairs.
sb_23_bio = sb_23_bio %>%
  mutate(StageCategory = as.factor(StageCategory))

levels(sb_23_bio$StageCategory) # levels of StageCategory

iti_50m_bio = iti_50m_bio %>%
  mutate(StageCategory = as.factor(StageCategory))

levels(iti_50m_bio$StageCategory) # levels of StageCategory

# limit to nests
sb_23_nests = sb_23_bio %>%
  filter(StageCategory == "chick" | StageCategory == "egg"| 
           StageCategory == "fledgling"| StageCategory == "nest"|
           StageCategory == "onegg" | StageCategory == "oneggs")%>%
  droplevels()

iti_50m_nests = iti_50m_bio %>%
  filter(StageCategory == "chick" | StageCategory == "egg"| 
           StageCategory == "fledgling"| StageCategory == "nest"|
           StageCategory == "onegg" | StageCategory == "oneggs")%>%
  droplevels()

# now calculate biomass and density per species per transect
sb_23_sp_transect = sb_23_nests %>%
  group_by(Motu, TransectStart, TransectEnd, SpeciesName, Body_mass_HBW) %>%
  summarize(BreedingPairs = sum(Nb, na.rm = TRUE),
            Abundance = BreedingPairs * 2) %>%  # multiply by 2 - assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance / 500, # density per m^2, # each transect is 5m x 100m (500 m^2)
         Density_ha = Density_m2 * 10000,  # convert to density per hectare
         biomass_kg = Abundance * Body_mass_HBW / 1000, # biomass in kg, divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg / 500, # biomass per m^2
         biomass_kg_ha = biomass_kg_m2 * 10000)# biomass per hectare

# do the same for the shorter Iti transects
iti_50m_sp_transect = iti_50m_nests %>%
  group_by(Motu, TransectStart, TransectEnd, SpeciesName, Body_mass_HBW) %>%
  summarize(BreedingPairs = sum(Nb, na.rm = TRUE),
            Abundance = BreedingPairs * 2) %>%  # multiply by 2 - assuming 2 adults per nest (so # of seabirds rather than # nests or breeding pairs)
  mutate(Density_m2 = Abundance / 250, # density per m^2, # each transect is 5m x 50m (250 m^2)
         Density_ha = Density_m2 * 10000,  # convert to density per hectare
         biomass_kg = Abundance * Body_mass_HBW / 1000, # biomass in kg, divide by 1000 to go from grams to kg
         biomass_kg_m2 = biomass_kg / 250, # biomass per m^2
         biomass_kg_ha = biomass_kg_m2 * 10000)# biomass per hectare

# now sum for total biomass at the transect level (rather than by species)
sb_23_transect = sb_23_sp_transect %>%
  group_by(Motu, TransectStart, TransectEnd)%>%
  summarize(breeding_density_ha_transect = sum(Density_ha),
            breeding_biomass_kgha_transect = sum(biomass_kg_ha))

iti_50m_transect = iti_50m_sp_transect %>%
  group_by(Motu, TransectStart, TransectEnd)%>%
  summarize(breeding_density_ha_transect = sum(Density_ha),
            breeding_biomass_kgha_transect = sum(biomass_kg_ha))

# add back in the gps coordinates that we lost along the way
sb_23_transect = left_join(sb_23_transect, 
                           select(sb_23, TransectStart, TransectEnd, 
                                  Latitude_Start, Longitude_Start, 
                                  Latitude_End, Longitude_End)) %>%
  distinct() %>%
  drop_na(Latitude_Start, Longitude_Start, 
          Latitude_End, Longitude_End)

# add back in the gps coordinates of the shorter Iti transects that we saved above
iti_50m_transect = left_join(iti_50m_transect, 
                             select(iti_50m_gps, TransectStart, TransectEnd, 
                                    Latitude_Start, Longitude_Start, 
                                    Latitude_End, Longitude_End)) %>%
  distinct() %>%
  drop_na(Latitude_Start, Longitude_Start, 
          Latitude_End, Longitude_End)

# bringing together ALL 2021 transect data
sb_23_transect = rbind(sb_23_transect, iti_50m_transect)

# converting from long format to wide format
sb_23_transect_long = sb_23_transect %>%
  # step 1: concatenate TransectStart and TransectEnd
  mutate(Transect = paste0(TransectStart, "to", TransectEnd)) %>%
  
  # step 2: pivot the Latitude and Longitude columns to long format
  pivot_longer(cols = c(Latitude_Start, Longitude_Start, Latitude_End, Longitude_End),
               names_to = c(".value","Coordinate_Type"),
               names_pattern = "(.*)_(Start|End)") %>%
  
  # step 3: rename columns as needed and rearrange them
  rename(Start_or_End = Coordinate_Type) %>%
  rename(Transect_Start = TransectStart) %>%
  rename(Transect_End = TransectEnd) %>%
  select(Motu, Transect, Transect_Start, Transect_End, 
         breeding_density_ha_transect, breeding_biomass_kgha_transect,
         Latitude, Longitude, Start_or_End) 

#### AVERAGE 2021 & 2023 DATA ####
# bring together the 2021 and 2023 seabird data
seabirds = rbind(mutate(sb_21_transect, Year = 2021),
                 mutate(sb_23_transect, Year = 2023))

# calculate the mean and standard deviation for breeding seabird biomass and density
# across the 2021 and 2023 sample periods
seabirds_avg = seabirds %>%
  group_by(Motu, TransectStart, TransectEnd) %>%
  summarise(avg_breeding_density_ha_transect = mean(breeding_density_ha_transect),
            std_breeding_density_ha_transect = sd(breeding_density_ha_transect),
            avg_breeding_biomass_kgha_transect = mean(breeding_biomass_kgha_transect),
            std_breeding_biomass_kgha_transect = sd(breeding_biomass_kgha_transect))

# join the summary statistics back to the original data
seabirds_gps_unique = seabirds %>%
  select(Motu, TransectStart, TransectEnd, 
         Latitude_Start, Longitude_Start, 
         Latitude_End, Longitude_End) %>%
  group_by(Motu, TransectStart, TransectEnd) %>%
  slice(1) %>%  # keep the first row per group to avoid duplicate gps records
  ungroup()

# perform the left join with the unique data
seabirds_avg = seabirds_avg %>%
  left_join(seabirds_gps_unique, by = c("Motu", "TransectStart", "TransectEnd"))

# converting from wide format to long format
seabirds_avg_long = seabirds_avg %>%
  # step 1: concatenate TransectStart and TransectEnd
  mutate(Transect = paste0(TransectStart, "to", TransectEnd)) %>%
  
  # step 2: pivot the Latitude and Longitude columns to long format
  pivot_longer(cols = c(Latitude_Start, Longitude_Start, Latitude_End, Longitude_End),
               names_to = c(".value","Coordinate_Type"),
               names_pattern = "(.*)_(Start|End)") %>%
  
  # step 3: rename columns as needed and rearrange them
  rename(Start_or_End = Coordinate_Type) %>%
  rename(Transect_Start = TransectStart) %>%
  rename(Transect_End = TransectEnd) %>%
  select(Motu, Transect, Transect_Start, Transect_End, 
         avg_breeding_density_ha_transect, std_breeding_density_ha_transect,
         avg_breeding_biomass_kgha_transect, std_breeding_biomass_kgha_transect,
         Latitude, Longitude, Start_or_End) 

# save the prepped breeding seabird data at the transect level
write.csv(seabirds_avg,
          here("Data", "Seabirds", "Summary_Seabird_Transect_Data_Wide_2021_2023.csv"),
          row.names = FALSE)

write.csv(seabirds_avg_long,
          here("Data", "Seabirds", "Summary_Seabird_Transect_Data_Long_2021_2023.csv"),
          row.names = FALSE)

#### INTERPOLATION ####
# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

# using the seabird data prepared above
seabird = seabirds_avg_long

# check and fix motu names in bird data as needed
unique(seabird$Motu) 
seabird$Motu = ifelse(seabird$Motu == "Hiraanae", "Hīra'a'ānae", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Tiaraunu", "Ti'ara'aunu", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Oroatera", "Horoāterā", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Aie", "'Ă'ie", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Auroa", "Ahuroa", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Rimatuu", "Rimatu'u", seabird$Motu)

# convert the tabular data to two separate points for each transect
seabird = seabird %>%
  # create unique IDs for each transect
  mutate(Transect_ID = paste(Motu, Transect, sep = "_"))

# separate Start and End points
start_points = seabird %>%
  filter(Start_or_End == "Start") %>%
  select(Transect_ID, Motu, Transect, Latitude, Longitude) %>%
  rename(Latitude_Start = Latitude, Longitude_Start = Longitude)

end_points = seabird %>%
  filter(Start_or_End == "End") %>%
  select(Transect_ID, Latitude, Longitude) %>%
  rename(Latitude_End = Latitude, Longitude_End = Longitude)

# join the start and end points into a single data frame
transects = 
  left_join(start_points, end_points)

# create start and end points as sf objects (CRS (WGS84))
transects_sf = transects %>%
  rowwise() %>%
  mutate(
    geometry = st_sfc(
      st_linestring(matrix(
        c(Longitude_Start, Latitude_Start, Longitude_End, Latitude_End),
        ncol = 2,
        byrow = TRUE)),
      crs = st_crs(4326))) %>%
  ungroup() %>%
  st_as_sf(crs = st_crs(4326)) %>%
  select(Transect_ID, Motu, Transect, geometry)

# transform to UTM Zone 6 South
transects_sf_utm = st_transform(transects_sf, crs = my_crs)
plot(transects_sf_utm)
# transect_lengths_m = transects_sf_utm %>%
#   mutate(Transect_Length_m = st_length(.))

# drop Ti'ara'aunu_1053to1069 because the length is suspiciously long indicating 
# that there is an error in the start and end GPS coordinates 
transects_sf_utm = transects_sf_utm %>%
  filter(!Transect_ID == "Ti'ara'aunu_1053to1069")
plot(transects_sf_utm)

# better, now add the seabird data
transects_sf_utm = left_join(transects_sf_utm, 
                             (seabird %>% 
                                select(Transect_ID,
                                       avg_breeding_density_ha_transect,
                                       std_breeding_density_ha_transect,
                                       avg_breeding_biomass_kgha_transect,
                                       std_breeding_biomass_kgha_transect)))
# save the transect lines for mapping later
transects_sf_utm %>%
  select(-Transect_ID) %>%
  rename(
    motu = Motu,
    transect = Transect,
    start = Transect_Start,
    avg_biom = avg_breeding_biomass_kgha_transect,  # Shortened
    std_biom = std_breeding_biomass_kgha_transect,  # Shortened
    avg_dens = avg_breeding_density_ha_transect,    # Shortened
    std_dens = std_breeding_density_ha_transect     # Shortened
  ) %>%
  st_write(., here("Data", "GIS", "Seabird_Transects.shp"), append = FALSE)

# isolate centroids of each transect
centroids = st_centroid(transects_sf_utm)

# open the topobathy raster made previously in 01_Preparing_Topobathy_Derivatives.R
topobathy = raster(here("Data", "Rasters", "TopoBathy.tif"))

# create a raster template based on topobathy
template_raster = raster(
  extent(topobathy),
  crs = crs(topobathy),
  resolution = res(topobathy))  

# ensure centroids are in the same CRS as the template raster
centroids = st_transform(centroids, st_crs(template_raster))

# ensure data has the 'avg_breeding_biomass_kgha_transect' column
# create SpatialPointsDataFrame with coordinates and data
centroids_sp = as_Spatial(centroids)

# convert template_raster to RasterLayer if it's a terra SpatRaster
if (inherits(template_raster, "SpatRaster")) {
  template_raster = raster(template_raster)
}

# check if centroids_sp has the required data
print(centroids_sp)

# define the IDW model with maxdist = 400
idw_model = gstat(
  id = "avg_breeding_biomass_kgha_transect",
  formula = avg_breeding_biomass_kgha_transect ~ 1,
  data = centroids_sp,
  nmin = 2,
  nmax = 5,
  maxdist = 400,
  set = list(idp = 1)
)

# perform interpolation with error handling
tryCatch({
  # interpolate to create the raster in memory (do not save to file yet)
  idw_raster = raster::interpolate(template_raster,
                                   idw_model, 
                                   ext = extent(topobathy))
  
  # replace NA values with 0
  idw_raster[is.na(idw_raster)] = 0
  
  # save the modified raster to file
  writeRaster(idw_raster, 
              filename = (here("Data", "Rasters", "Seabird_Biomass_IDW.tif")), 
              overwrite = TRUE)
  
  # plot the result
  plot(idw_raster, main = "IDW Interpolation of Breeding Biomass (0 beyond 400 m)")
  
}, error = function(e) {
  print("Error during interpolation:")
  print(e)
})

# done