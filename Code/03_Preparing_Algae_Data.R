#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("raster", "terra", "sp", "sf", "dplyr", "conflicted",
#                    "spatialEco", "PNWColors", "here", "readxl", "nngeo"))
library(easypackages)
libraries("raster", "terra", "sp", "sf", "dplyr", "conflicted", "spatialEco", 
          "PNWColors", "here", "readxl", "nngeo")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("extract", "raster")
rasterOptions(progress = 'text') # progress info for processing large rasters
options(terra.progress = 1) # progress info for processing large rasters
rasterOptions(tmpdir = "E:/temp_raster_directory") # custom directory for temporary files
terraOptions(tempdir = "E:/temp_raster_directory") # custom directory for temporary files

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch1/")
# set_here("E:/Data/StuartC_DPhil_Ch1/") set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package


#### PROJ.4 STRINGS ####
# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")


#### COMBINE ALGAE DATASETS ####
# ATA data collected in November 2021 by Hannah Epstein, Kal Bistolas, 
# Casey Benkwitt, Jayna DeVore, and Carlee Heger
ata2021 = read_xlsx(here("Data", "Algae", "ATA_Tetiaroa_November_2021.xlsx"),
                    sheet = "ATA_Nov2021")
# cleaning
ata2021 = ata2021 %>%
  rename(Sample = vial,
         Longitude_Degrees = Longitude,
         Latitude_Degrees = Latitude) %>%
  mutate(Month = case_when(
    grepl("Nov", Month) ~ "November",
    TRUE ~ NA_character_))

# transect data collected in November 2021 by Hannah Epstein
transect2021 = read_xlsx(here("Data", "Algae", "Transects_Tetiaroa_November_2021.xlsx"),
                         sheet = "Nov2021_Transect_data")
# cleaning
transect2021 = transect2021 %>%
  rename(Sample = vial,
         Longitude_Degrees = Longitude,
         Latitude_Degrees = Latitude) %>%
  mutate(Month = case_when(
    grepl("Nov", Month) ~ "November",
    TRUE ~ NA_character_))

# transect data collected in September 2023 by Courtney Stuart
transect2023 = read_xlsx(here("Data", "Algae", "StuartC_Algae_BGS_2024_Tetiaroa_2023_Results.xlsx"),
                         sheet = "Samples")
# cleaning
transect2023 = transect2023 %>%
  rename(N15 = `δ¹⁵N (‰ Air)`,
         C13 = `δ¹³C (‰ VPDB)`,
         N_percent = `N %`,
         C_percent = `C %`,
         Species = `Algae Species`) %>%
  filter(Species == "Turbinaria ornata")
unique(transect2023$Identifier)
transect2023 = transect2023 %>%
  filter(!grepl("OUTFLOW", Identifier)) %>%
  select(-`...5`)

# add in the 2023 GPS data
gps2023 = read.csv(here("Data", "Algae", "StuartC_Tetiaroa_2023_Algae_GPS.csv"))
# cleaning
gps2023 = gps2023 %>%
  rename(Longitude_Degrees = Longitude,
         Latitude_Degrees = Latitude,
         Identifier = ident) %>%
  mutate(Collected_by = "Courtney Stuart (Oxford)",
         Month = "September",
         Year = "2023",
         Purpose = "Transect")

# add columns for motu and sampling distance along transect
gps2023 = gps2023 %>%
  filter(!grepl("OUTFLOW", Identifier)) %>%
  filter(!grepl("outflow", Identifier)) %>%
  filter(!grepl("HAL", Identifier)) %>%
  mutate(Motu = case_when(
    grepl("ONE", Identifier) ~ "Onetahi",
    grepl("HON", Identifier) ~ "Honuea",
    grepl("TIA", Identifier) ~ "Tiaraunu",
    grepl("TAU", Identifier) ~ "Tauini",
    grepl("AUR", Identifier) ~ "Auroa",
    grepl("HIR", Identifier) ~ "Hiraanae",
    grepl("ORO", Identifier) ~ "Oroatera",
    grepl("AIE", Identifier) ~ "Aie",
    grepl("REI", Identifier) ~ "Reiono",
    grepl("RIM", Identifier) ~ "Rimatuu",
    TRUE ~ NA_character_)) %>%
  mutate(Side = case_when(
    grepl("lee", Identifier) ~ "Leeward",
    grepl("wind", Identifier) ~ "Windward",
    TRUE ~ NA_character_)) %>%
  mutate(Transect = case_when(
    grepl("01", Identifier) ~ 01,
    grepl("02", Identifier) ~ 02,
    grepl("03", Identifier) ~ 03,
    grepl("04", Identifier) ~ 04,
    grepl("05", Identifier) ~ 05,
    grepl("06", Identifier) ~ 06,
    grepl("07", Identifier) ~ 07,
    grepl("08", Identifier) ~ 08,
    grepl("09", Identifier) ~ 09,
    grepl("10", Identifier) ~ 10,
    grepl("11", Identifier) ~ 11,
    TRUE ~ NA_real_)) %>%
  mutate(Distance = case_when(
    grepl("A$", Identifier) ~ 10,
    grepl("B$", Identifier) ~ 20,
    grepl("C$", Identifier) ~ 30,
    grepl("D$", Identifier) ~ 40,
    TRUE ~ NA_real_))

# remove unnecessary columns 
gps2023 = select(gps2023, Identifier, Motu, Side, Transect, Distance,
                 Longitude_Degrees, Latitude_Degrees, Collected_by,
                 Month, Year, Purpose)
# combine 2023 algae and GPS data
transect2023 = left_join(transect2023, gps2023,
                         by = c("Sample" = "Identifier"))

# 2023 samples were processed in duplicate, so calculate mean N15, C13, N_percent, C_percent
transect2023 = transect2023 %>%
  select(-Sample) %>%
  group_by(Year, Month, Purpose, Collected_by, Identifier, Motu, Side, 
           Transect, Distance, Longitude_Degrees, Latitude_Degrees) %>%
  summarise(N15 = mean(N15),
            C13 = mean(C13),
            N_percent = mean(N_percent),
            C_percent = mean(C_percent)) %>%
  rename(Sample = Identifier) %>%
  ungroup()

# combine the 2021 and 2023 algae datasets
all_algae = rbind(
  (select(ata2021, Month, Year, Motu, Purpose, Collected_by,
          Longitude_Degrees, Latitude_Degrees, Sample, 
          N15, C13, N_percent, C_percent)),
  (select(transect2021, Month, Year, Motu, Purpose, Collected_by,
          Longitude_Degrees, Latitude_Degrees, Sample, 
          N15, C13, N_percent, C_percent)),
  (select(transect2023, Month, Year, Motu, Purpose, Collected_by,
          Longitude_Degrees, Latitude_Degrees, Sample, 
          N15, C13, N_percent, C_percent)))

# remove sites that had no algae present
all_algae = all_algae %>%
  filter(!is.na(N15) & !is.na(C13) & !is.na(N_percent) & !is.na(C_percent))

# check motu names across years for consistency
unique(all_algae$Motu)

# fix some of the motu names for consistency and alignment with Tahitian place names
all_algae$Motu = ifelse(all_algae$Motu == "Hiranae", "Hīra'a'ānae", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Tiaraunu Hoa", "Tauini", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Tiaraunu", "Ti'ara'aunu", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Oroatera", "Horoāterā", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Aie", "'Ă'ie", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Auroa", "Ahuroa", all_algae$Motu)
all_algae$Motu = ifelse(all_algae$Motu == "Rimatuu", "Rimatu'u", all_algae$Motu)

# check again
unique(all_algae$Motu) 

#### ADD BIOGEOPHYSICAL DATA ####
# add the seabird data previously prepared
seabird = read.csv(here("Data", "Seabirds", "Seabird_Summary_Data_2021_2023.csv"))

# check and fix motu names in bird data as needed
unique(seabird$Motu) 
seabird$Motu = ifelse(seabird$Motu == "Hiraanae", "Hīra'a'ānae", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Tiaraunu", "Ti'ara'aunu", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Oroatera", "Horoāterā", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Aie", "'Ă'ie", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Auroa", "Ahuroa", seabird$Motu)
seabird$Motu = ifelse(seabird$Motu == "Rimatuu", "Rimatu'u", seabird$Motu)

# add the seabird data to the algae data as new columns 
all_algae = left_join(all_algae, seabird, by = "Motu")

# checking for any NAs across the data
summary(all_algae)

# there are NAs in the two seabird standard deviation columns - this is ok - because
# Honuea was only sampled in one year, so there is nothing to calculate standard deviation
# from

# future models will only predict nutrient conditions within 400m of motu edges. 
# this is based on previous papers which have provided evidence  of seabird nutrient
# assimilation in algae, coral, and other marine endmembers up to 100s of meters offshore.
# see references: 
# Lorrain et al., 2017, Scientific Reports 7: 3721→ Surprise and Reynard, New Caledonia
# Graham et a., 2018, Nature, 559: 250-253 → Chagos Archipelago
# Savage et al., 2019, Scientific Reports, 9:4284 → Namena & Cousteau, Fiji Arhcipelago
# Benkwitt et al., 2021, Current Biology 31, 2704–2711 → Scattered Islands & Chagos Archipelago

# open the motu shapefile made previously
motus = st_read(here("Data", "GIS", "Motus.shp"))

# create a 400-m buffer around each motu
motu_buffer = st_buffer(motus, dist = 400)

# adjust place names
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Hiraanae", "Hīra'a'ānae", motu_buffer$Motu)
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Tiaraunu", "Ti'ara'aunu", motu_buffer$Motu)
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Oroatera", "Horoāterā", motu_buffer$Motu)
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Aie", "'Ă'ie", motu_buffer$Motu)
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Auroa", "Ahuroa", motu_buffer$Motu)
motu_buffer$Motu = ifelse(motu_buffer$Motu == "Rimatuu", "Rimatu'u", motu_buffer$Motu)

# add the seabird data associated with each motu
motu_buffer = motu_buffer %>%
  left_join(seabird, by = "Motu")

# create an empty raster with the same extent and CRS as motu_buffer
raster_template = raster(
  extent(motu_buffer),       # use the extent of motu_buffer
  crs = st_crs(motu_buffer)$proj4string, # use the CRS of motu_buffer
  resolution = c(0.3, 0.3)       # set the resolution to 3 x 3 meters
)

# rasterize the motu_buffer polygons, using avg_breeding_biomass_kgha_motu 
# as the value. !!! NOTE !!! the rasterize function will take roughly one
# hour to run due to the fine spatial resolution of the raster template. 
writeRaster((rasterize(
  motu_buffer,  # spatial polygons
  raster_template,  # raster template
  field = "avg_breeding_biomass_kgha_motu",  # column with the values to assign
  fun = 'mean')), # function to handle multiple polygons per cell
            here("Data", "Rasters", "Seabird_Biomass_400m.tif"),
            overwrite = TRUE)
# read back in the raster to confirm it worked
motu_raster = raster(here("Data", "Rasters", "Seabird_Biomass_400m.tif"))
compareCRS(motu_raster, my_crs) # should be TRUE
res(motu_raster) # should be 0.3 x 0.3 m
plot(motu_raster) # take a look

#### BRING TOGETHER ALGAE AND BIOPHYSICAL DATA ####
# convert the tabular algae data to spatial data and transform to 
# desired CRS
algae_sf = all_algae %>%
  st_as_sf(., coords = c(6, 7), crs = gcs) %>% # coords(lon,lat)
  st_transform(., my_crs) # re-project 

# read in the other biogeophysical rasters previously created
topobathy = raster(here("Data", "Rasters", "TopoBathy.tif"))
slope = raster(here("Data", "Rasters", "QSlope.tif"))
aspect = raster(here("Data", "Rasters", "QAspect.tif"))
eastness = raster(here("Data", "Rasters", "QEastness.tif"))
northness = raster(here("Data", "Rasters", "QNorthness.tif"))
planc = raster(here("Data", "Rasters", "QPlanCurve.tif"))
profc = raster(here("Data", "Rasters", "QProfCurve.tif"))
meanc = raster(here("Data", "Rasters", "QMeanCurve.tif"))
sapa = raster(here("Data", "Rasters", "SAPARugosity.tif"))
landdist = raster(here("Data", "Rasters", "LandDist.tif"))
onetahidist = raster(here("Data", "Rasters", "OnetahiDist.tif"))
habitat = raster(here("Data", "Rasters", "Habitat_2m.tif"))

# check that they all align, have the same CRS and resolution
compareRaster(x = c(topobathy, slope, aspect, eastness, northness, 
                    profc, planc, meanc, sapa, landdist, onetahidist),
              extent = T, crs = T, rowcol = T, res = T)
compareCRS(habitat, topobathy)

# resample the habitat raster to have 30cm by 30cm resolution and the same number of
# rows and columns as the other predictors. this will NOT add any new data or 
# information relative to the 2m by 2m raster, but will enable us to add the habitat
# raster to a raster stack with the other predictors. we can then use the stack to
# easily extract the value of all predictors at our algae sampling points using one 
# line of code. save the resampled raster to our folder of temporary files.
habitat = resample(x = habitat_ras,
                   y = topobathy,
                   method = "ngb")
writeRaster(habitat, here("Temp", "Habitat_Resampled_30cm.tif"))
#habitat = raster(here("Temp", "Habitat_Resampled_30cm.tif"))
compareRaster(habitat, topobathy, extent = T, crs = T, rowcol = T, res = T)

# clip the other rasters to also have an extent only up to 400m offshore
depth_400m = mask(crop(topobathy, extent(motu_buffer)), 
                  as(motu_buffer, "Spatial"))
# resampling seabird data so it aligns with the others
seabird_biomass_400m = crop(resample(motu_raster, depth_400m,
                                     method = "bilinear"), extent(depth_400m))
# clipping
slope_400m = mask(crop(slope, extent(motu_buffer)), 
                  as(motu_buffer, "Spatial"))
plancurv_400m = mask(crop(planc, extent(motu_buffer)), 
                     as(motu_buffer, "Spatial"))
profcurv_400m = mask(crop(profc, extent(motu_buffer)), 
                     as(motu_buffer, "Spatial"))
meancurv_400m = mask(crop(meanc, extent(motu_buffer)), 
                     as(motu_buffer, "Spatial"))
aspect_400m = mask(crop(aspect, extent(motu_buffer)), 
                   as(motu_buffer, "Spatial"))
eastness_400m = mask(crop(eastness, extent(motu_buffer)), 
                     as(motu_buffer, "Spatial"))
northness_400m = mask(crop(northness, extent(motu_buffer)), 
                      as(motu_buffer, "Spatial"))
sapa_400m = mask(crop(sapa, extent(motu_buffer)), 
                 as(motu_buffer, "Spatial"))
landdist_400m = mask(crop(landdist, extent(motu_buffer)), 
                     as(motu_buffer, "Spatial"))
onetahidist_400m = mask(crop(onetahidist, extent(motu_buffer)), 
                        as(motu_buffer, "Spatial"))
habitat_400m = raster::mask(raster::crop(habitat, extent(motu_buffer)), 
                            as(motu_buffer, "Spatial"))

# compare the resolutions, extents, row/columns, and CRSs of all rasters
# this should print out "TRUE" if the rasters are properly aligned
compareRaster(c(depth_400m, slope_400m, plancurv_400m, profcurv_400m,
                meancurv_400m, aspect_400m, eastness_400m, 
                northness_400m, sapa_400m, landdist_400m, 
                onetahidist_400m, seabird_biomass_400m, habitat_400m), 
              extent = T, crs = T, rowcol = T, res = T)

# save the predictor rasters within 400-m buffer of each motu
writeRaster(depth_400m, here("Data", "Rasters", "Depth_400m.tif"), overwrite = TRUE)
writeRaster(slope_400m, here("Data", "Rasters", "Slope_400m.tif"), overwrite = TRUE)
writeRaster(plancurv_400m, here("Data", "Rasters", "QPlanCurve_400m.tif"), overwrite = TRUE)
writeRaster(profcurv_400m, here("Data", "Rasters", "QProfCurve_400m.tif"), overwrite = TRUE)
writeRaster(meancurv_400m, here("Data", "Rasters", "QMeanCurve_400m.tif"), overwrite = TRUE)
writeRaster(aspect_400m, here("Data", "Rasters", "QAspect_400m.tif"), overwrite = TRUE)
writeRaster(eastness_400m, here("Data", "Rasters", "QEastness_400m.tif"), overwrite = TRUE)
writeRaster(northness_400m, here("Data", "Rasters", "QNorthness_400m.tif"), overwrite = TRUE)
writeRaster(sapa_400m, here("Data", "Rasters", "SAPARugosity_400m.tif"), overwrite = TRUE)
writeRaster(landdist_400m, here("Data", "Rasters", "LandDist_400m.tif"), overwrite = TRUE)
writeRaster(onetahidist_400m, here("Data", "Rasters", "OnetahiDist_400m.tif"), overwrite = TRUE)
writeRaster(habitat_400m, here("Data", "Rasters", "Habitat_400m.tif"), overwrite = TRUE)
writeRaster(seabird_biomass_400m, here("Data", "Rasters", "Seabird_Biomass_400m.tif"),
            overwrite = TRUE)

# remove the large, full rasters that cover the entirety of Tetiaroa
# only work with those up to 400m offshore moving forward
rm(list = c("topobathy", "slope", "meanc", "planc", "profc", "aspect",
            "eastness", "northness", "sapa", "landdist", "onetahidist",
            "motu_raster", "habitat"))

# create a raster stack with all desired predictors
env = stack(x = c(habitat_400m, depth_400m, slope_400m, aspect_400m,
                  eastness_400m, northness_400m, meancurv_400m, 
                  plancurv_400m, profcurv_400m, sapa_400m,landdist_400m,
                  onetahidist_400m, seabird_biomass_400m))
# assign names to layers in stack
names(env) = c("habitat_400m", "depth_400m", "slope_400m", "aspect_400m",
               "eastness_400m", "northness_400m", "meancurv_400m", 
               "plancurv_400m", "profcurv_400m", "sapa_400m",
               "landdist_400m", "onetahidist_400m", "seabird_biomass_400m")

# extract values of predictors at all algae sampling points
algae_sp = as(algae_sf, "Spatial")
compareCRS(algae_sp, env) # should be TRUE
class(algae_sp)  # should be "SpatialPointsDataFrame"
class(env)       # should be "RasterStack" or "RasterBrick"
algae_w_predictors = cbind(algae_sp, extract(env, algae_sp)) # extracting
algae_w_predictors = as.data.frame(algae_w_predictors) # save as dataframe

# some quick renaming
algae_w_predictors = algae_w_predictors %>%
  rename(Avg_Breeding_Bird_Density = avg_breeding_density_ha_motu,
         SD_Breeding_Bird_Density = std_breeding_density_ha_motu,
         Avg_Breeding_Bird_Biomass = avg_breeding_biomass_kgha_motu,
         SD_Breeding_Bird_Biomass = std_breeding_biomass_kgha_motu,
         Habitat_Resampled_30cm = habitat_400m,
         Depth = depth_400m,
         Slope = slope_400m,
         Aspect = aspect_400m,
         Eastness = eastness_400m,
         Northness = northness_400m,
         Plan_Curve = plancurv_400m,
         Prof_Curve = profcurv_400m,
         Mean_Curve = meancurv_400m,
         SAPA_Rugosity = sapa_400m,
         Land_Distance = landdist_400m, 
         Onetahi_Distance = onetahidist_400m,
         Seabird_Biomass = seabird_biomass_400m,
         Longitude_UTM6S = coords.x1,
         Latitude_UTM6S = coords.x2)

# add a column with the habitat class names, using the integer codes saved previously
hab_cod_df = read.csv(here("Data", "Habitat", "Habitat_Class_Raster_Codes.csv"))
algae_w_predictors = left_join(algae_w_predictors, hab_cod_df,
                               by = c("Habitat_Resampled_30cm" = "Hab_Cod")) %>%
  relocate(Habitat, .after = Habitat_Resampled_30cm) # place it after the habitat integer column

# add the coarse habitat classifications as well
coarse = read.csv(here("Data", "Habitat", "StuartC_Coarse_Habitat_Classes.csv"))
algae_w_predictors = left_join(algae_w_predictors, coarse,
                               by = c("Habitat" = "FPSOTE_habitats_2m")) %>%
  rename(Coarse_Habitat = StuartC_Coarse_Habitat_Classes) %>%
  relocate(Coarse_Habitat, .after = "Habitat") %>%
  mutate(Coarse_Habitat_ID = as.numeric(factor(Coarse_Habitat, 
                                               levels = unique(Coarse_Habitat)))) %>%
  relocate(Coarse_Habitat_ID, .before = "Coarse_Habitat")

# remove rows where topobathy values are positive (above 0m)...although algae 
# were clearly collected here and these sites may be right along the beach line, 
# I don't want to confuse the model by providing both (+) elevations and (-) depths.
# this will only remove 4 records, which is negligible in this case.
algae_w_predictors = algae_w_predictors %>%
  filter(Depth < 0)

# do any of the algae collection sites say they're in terrestrial vegetation? 
unique(algae_w_predictors$Habitat)

# three records shows up as terrestrial, but our original habitat and remote 
# sensing imagery show that they're actually submerged rock. this issue is probably
# an artifact of the rasterization process and can be overwritten as rock. 
algae_w_predictors = algae_w_predictors %>%
  rename(Fine_Habitat_ID = Habitat_Resampled_30cm,
         Fine_Habitat = Habitat)

# fixing the fine habitat data (categorical)
algae_w_predictors$Fine_Habitat = 
  ifelse(algae_w_predictors$Fine_Habitat == "Terrestrial vegetation", "Rock",
         algae_w_predictors$Fine_Habitat)
unique(algae_w_predictors$Fine_Habitat) # done

# fixing the fine habitat data (integer)
algae_w_predictors$Fine_Habitat_ID = 
  ifelse(algae_w_predictors$Fine_Habitat_ID == 29, 34,
         algae_w_predictors$Fine_Habitat_ID)
unique(algae_w_predictors$Fine_Habitat_ID) # done

# fixing the coarse habitat data (categorical)
algae_w_predictors$Coarse_Habitat = 
  ifelse(algae_w_predictors$Coarse_Habitat == "Terrestrial vegetation", "Rock",
         algae_w_predictors$Coarse_Habitat)
unique(algae_w_predictors$Coarse_Habitat) # done

# fixing the coarse habitat data (integer)
algae_w_predictors$Coarse_Habitat_ID = 
  ifelse(algae_w_predictors$Coarse_Habitat_ID == 4, 2,
         algae_w_predictors$Coarse_Habitat_ID)
unique(algae_w_predictors$Coarse_Habitat_ID) # done

# move the longitude and latitude information before the Sample column
algae_w_predictors = algae_w_predictors %>%
  relocate(Latitude_UTM6S, .before = Sample) %>%
  relocate(Longitude_UTM6S, .before = Latitude_UTM6S) %>%
  relocate(Coarse_Habitat_ID, .before = Coarse_Habitat)

# make sure that the motu names are saved properly
Encoding(algae_w_predictors$Motu) = "UTF-8"

# also add a new motu column that does not use special characters in case 
# the encoding in the line above does not work
replace_special_chars = function(text) {
  text = gsub("'Ă", "A", text)     
  text = gsub("ī", "i", text)      
  text = gsub("'", "", text)  
  text = gsub("ā", "a", text)
  return(text)}
algae_w_predictors = algae_w_predictors %>%
  mutate(Motu_No_Special = replace_special_chars(Motu)) %>% 
  relocate(Motu_No_Special, .after = Motu)

# there is only one record of back reef coral bommies in our dataset
print(filter(algae_w_predictors, Fine_Habitat == "Back reef coral bommies"))
# drop this observation, as we won't be able to make predictions on it
# because back reef coral bommies will not be in the training data when
# later using leave one out cross validation. Also drop records of algae
# collected over 70m from shore (these are likely an error as sampling did
# not take place this far from shore).

algae_w_predictors = algae_w_predictors %>%
  filter(!Fine_Habitat == "Back reef coral bommies") %>%
  filter(Land_Distance < 70)

# finally save the cleaned and prepped algae-environment data as a csv
write.csv(algae_w_predictors, 
          here("Data", "Model_Data", "Cleaned_Tetiaroa_2021_2023_Data.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# and save the working environment
save.image(here("Code", "03_Preparing_Algae_Data.RData"))
