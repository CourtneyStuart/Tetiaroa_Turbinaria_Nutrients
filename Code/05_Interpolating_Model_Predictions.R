#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "tidyverse", "ggplot2", "PNWColors",
#                   "readxl", "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
#                   "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats", "DHARMa", "see",
#                   "gbm", "spdep", "gstat", "here", "gratia", "raster", "visibly",
#                   "parallel", "doParallel", "sf", "sp", "ggplot2"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
          "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats",
          "DHARMa", "see", "gbm", "spdep", "gstat", "here", "gratia",
          "raster", "visibly", "parallel", "terra", "doParallel",
          "sf", "sp", "ggplot2")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("relocate", "dplyr")
conflicted::conflict_prefer("partial", "pdp")
conflicted::conflict_prefer("draw", "gratia")
rasterOptions(progress = 'text') # progress info for processing large rasters
options(terra.progress = 1) # progress info for processing large rasters
rasterOptions(tmpdir = "F:/temp_raster_directory") # custom directory for temporary files
terraOptions(tempdir = "F:/temp_raster_directory") # custom directory for temporary files


#### DIRECTORIES ####
# working directory and relative folder path
setwd("F:/Data/StuartC_DPhil_Ch1/")
# set_here("F:/Data/StuartC_DPhil_Ch1/") set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

# set seed for future
set.seed(123)

# load working environment from modelling script
load(here("Code", "04_Modelling_Nutrientscape.RData"))

# we only need the fitted model objects, model results, and our project's coordinate
# reference information for this script
rm(list = setdiff(ls(), c("GAM1", "GAM2", "results", "my_crs")))

# read in the rasters that represent our variables (up to 400m offshore)
depth = rast(here("Data", "Rasters", "Depth_400m.tif"))
slope = rast(here("Data", "Rasters", "Slope_400m.tif"))
plancurv = rast(here("Data", "Rasters", "QPlanCurve_400m.tif"))
profcurv = rast(here("Data", "Rasters", "QProfCurve_400m.tif"))
eastness = rast(here("Data", "Rasters", "QEastness_400m.tif"))
northness = rast(here("Data", "Rasters", "QNorthness_400m.tif"))
landdist = rast(here("Data", "Rasters", "LandDist_400m.tif"))
fine_habitat = rast(here("Data", "Rasters", "Habitat_400m.tif"))
seabird_biomass = rast(here("Data", "Rasters", "Seabird_Biomass_IDW_400m.tif"))

# we won't be making predictions over land at all so exclude all cells with a
# a depth >= 0.5 m (the tidal range of Tetiaroa)
# create a new raster where areas with values >= 0.5 are set to NA
motu = st_read(here("Data", "GIS", "Motus.shp"))
compareCRS(motu, my_crs)
depth_clipped = mask(depth, motu, inverse = TRUE)
#plot(depth_clipped)

# do the same for all other rasters
rasters_list = list(
  slope = slope,
  plancurv = plancurv,
  profcurv = profcurv,
  eastness = eastness,
  northness = northness,
  landdist = landdist,
  fine_habitat = fine_habitat,
  seabird_biomass = seabird_biomass)
names(rasters_list)

# loop through each raster in the list
for (name in names(rasters_list)) {
  # get the current raster
  current_raster = rasters_list[[name]]
  
  # clip the raster using depth_clipped as the mask
  clipped_raster = current_raster
  clipped_raster[is.na(depth_clipped)] = NA
  
  # create a new variable name with suffix "_clipped"
  new_name = paste0(name, "_clipped")
  
  # save the clipped raster to the environment
  assign(new_name, clipped_raster, envir = .GlobalEnv)
  
  # run garbage collection to free up memory
  gc()
}

# check a few results to confirm that this worked
plot(plancurv_clipped)
plot(fine_habitat_clipped)

# remember that our models included coarse habitat (three classes), rather than
# the fine habitat. we need to update our habitat raster before moving forward.
# create a data frame for reclassification
reclass_table = data.frame(
  FPSOTE_habitats_2m = c("Shallow fore reef slope", "Shallow fore reef terrace", "Coralline algal ridge", "Deep fore reef slope",
                         "Back reef coral framework", "Back reef coral bommies", "Back reef sediment dominated", "Back reef rubble dominated",
                         "Back reef pavement", "Rock", "Beach sand", "Coral rubble", "Terrestrial vegetation", "Lagoonal pinnacle reefs massive coral dominated",
                         "Lagoonal floor barren", "Lagoonal patch reefs", "Lagoonal floor coral bommies", "Lagoonal sediment apron sediment dominated",
                         "Deep lagoonal water", "Urban", "Lagoonal floor macroalgae on sediment", "Area not mapped"),
  
  Hab_Cod = c(2, 1, 5, 3, 9, 10, 7, 6, 8, 34, 30, 35, 29, 17, 14, 20, 21, 12, 28, 33, 15, 0),
  
  StuartC_Coarse_Habitat_Classes = c("Coral reef and hardbottom", "Coral reef and hardbottom", "Coral reef and hardbottom", "Coral reef and hardbottom",
                                     "Coral reef and hardbottom", "Coral reef and hardbottom", "Unconsolidated sediment", "Coral reef and hardbottom",
                                     "Coral reef and hardbottom", "Rock", "Unconsolidated sediment", "Coral reef and hardbottom", "Terrestrial vegetation",
                                     "Coral reef and hardbottom", "Unconsolidated sediment", "Coral reef and hardbottom", "Coral reef and hardbottom",
                                     "Unconsolidated sediment", "Deep lagoonal water", "Urban", "Unconsolidated sediment", "Area not mapped"),
  
  StuartC_Code = c(3, 3, 3, 3, 3, 3, 1, 3, 3, 2, 1, 3, NA, 3, 1, 3, 3, 1, NA, NA, 1, NA) # Codes based on provided mappings
)

# keep only the IDs, as that's what's in our raster
reclass_table = reclass_table[, c("Hab_Cod", "StuartC_Code")]
colnames(reclass_table) = c("from", "to")
reclass_table$from = as.numeric(reclass_table$from)
reclass_table$to = as.numeric(reclass_table$to)

# convert the habitat reclassification table to a matrix
reclass_matrix = as.matrix(reclass_table)

# reclassify the habitat raster
coarse_habitat_clipped = reclassify(raster(fine_habitat_clipped),
                                    rcl = reclass_matrix)
#par(mar=c(0,0,0,0))
#plot(coarse_habitat_clipped)

# before moving on, make sure all rasters have the same CRS, extent, and resolution
compareRaster(c(raster(depth_clipped), raster(slope_clipped), 
                raster(plancurv_clipped), raster(profcurv_clipped),
                raster(eastness_clipped), raster(northness_clipped),
                raster(landdist_clipped), coarse_habitat_clipped, 
                raster(seabird_biomass_clipped)),
              extent = TRUE, res = TRUE, orig = TRUE, crs = TRUE) # should be TRUE

# remove the full rasters, keep only the clipped ones
rm(list = c("depth", "slope", "plancurv", "profcurv", "eastness", "northness",
            "landdist",  "fine_habitat", "fine_habitat_clipped", "seabird_biomass"))

# we now need to deal with the 2D smooth of spatial coordinates
# create longitude and latitude rasters using init(), pull from the depth_clipped raster
longitude_raster = raster::init(depth_clipped, fun = 'x')  # fill with longitude (x) values
latitude_raster = raster::init(depth_clipped, fun = 'y')   # fill with latitude (y) values

# clip out the land areas
longitude_clipped = mask(longitude_raster, depth_clipped)
latitude_clipped = mask(latitude_raster, depth_clipped)
plot(longitude_clipped, main = "Longitude Raster")
plot(latitude_clipped, main = "Latitude Raster")

# keeo the clipped lat-long rasters only
rm(list = c("longitude_raster", "latitude_raster"))

# we now need to deal with the factors in our model...
# create a constant year raster, we'll use 2023 because it's the most recent
year_clipped = latitude_clipped

# fill the entire raster with the value 2023
values(year_clipped) = 2023

# convert the habitat and year rasters to factors
coarse_habitat_clipped = as.factor(coarse_habitat_clipped)
year_clipped = as.factor(year_clipped)

#### GAM1 PREDICTIONS ####
# check variable names in the model itself to ensure a match
names(GAM1$var.summary)

# stack all predictors together
predictors_stack_raster = stack(coarse_habitat_clipped, 
                                raster(year_clipped),
                                raster(seabird_biomass_clipped), 
                                raster(depth_clipped), 
                                raster(slope_clipped), 
                                raster(eastness_clipped), 
                                raster(northness_clipped), 
                                raster(plancurv_clipped), 
                                raster(profcurv_clipped),
                                raster(landdist_clipped), 
                                raster(longitude_clipped), 
                                raster(latitude_clipped))

# assign correct names to each raster in the stack
names(predictors_stack_raster) = c("Coarse_Habitat_ID", 
                                   "Year",
                                   "Seabird_Biomass", 
                                   "Depth", 
                                   "Slope", 
                                   "Eastness", 
                                   "Northness", 
                                   "Plan_Curve", 
                                   "Prof_Curve",
                                   "Land_Distance", 
                                   "Longitude_UTM6S", 
                                   "Latitude_UTM6S")

num_cores = detectCores()-4  # adjust this to the number of cores you want to use

# register the parallel backend
cl = makeCluster(num_cores)           # create a cluster with the desired number of cores
registerDoParallel(cl)                 # register the cluster for parallel processing
# use raster::predict with parallel processing
nutrientscape_GAM1 = raster::predict(predictors_stack_raster, 
                                     GAM1, 
                                     type = "response", 
                                     na.rm = TRUE, 
                                     progress = "text",  # show progress
                                     cores = num_cores)  # number of cores

# save the results
writeRaster(nutrientscape_GAM1,
            here("Data", "Model_Data", "Nutrientscape_GAM1.tif"))

stopCluster(cl)  # stop the parallel cluster

#### GAM2 PREDICTIONS ####
# check variable names in the model itself to ensure a match
names(GAM2$var.summary)

# stack all predictors together
predictors_stack_raster = stack(raster(year_clipped),
                                raster(seabird_biomass_clipped), 
                                raster(depth_clipped),
                                raster(landdist_clipped), 
                                raster(longitude_clipped), 
                                raster(latitude_clipped))

# assign correct names to each raster in the stack
names(predictors_stack_raster) = c("Year",
                                   "Seabird_Biomass", 
                                   "Depth", 
                                   "Land_Distance", 
                                   "Longitude_UTM6S", 
                                   "Latitude_UTM6S")

num_cores = detectCores()-4

# register the parallel backend
cl = makeCluster(num_cores)           # create a cluster with the desired number of cores
registerDoParallel(cl)                 # register the cluster for parallel processing
# use raster::predict with parallel processing
nutrientscape_GAM2 = raster::predict(predictors_stack_raster, 
                                     GAM2, 
                                     type = "response", 
                                     na.rm = TRUE, 
                                     progress = "text",  # show progress
                                     cores = num_cores)  # number of cores

# save the results
writeRaster(nutrientscape_GAM2,
            here("Data", "Model_Data", "Nutrientscape_GAM2.tif"))

stopCluster(cl)  # stop the parallel cluster

#### CLAMPING ####
# clamp the predictions to a δ¹⁵N range of 0- to +30‰, as GAMs with spatial smooths may
# extrapolate poorly when making predictions for geographic locations with conditions 
# that differ substantially from those in the training data. this approach mitigates the 
# risk of generating implausible δ¹⁵N values due to the model's reliance on localised
# regression techniques, which may not accurately capture conditions outside the spatial 
# extent of the observed data.

# clamp predictions
nutrientscape_GAM1 = raster(here("Data", "Model_Data", "Nutrientscape_GAM1.tif"))
nutrientscape_GAM2 = raster(here("Data", "Model_Data", "Nutrientscape_GAM2.tif"))

# define the clamping function
clamp_values = function(x, min_val, max_val) {
  pmin(pmax(x, min_val), max_val)
}

# apply clamping to GAM1
nutrientscape_GAM1_clamped =
  raster::calc(nutrientscape_GAM1, 
               fun = function(x) clamp_values(x, min_val = 0, max_val = 30))

# save the results
writeRaster(nutrientscape_GAM1_clamped, overwrite = TRUE, 
            here("Data", "Model_Data", "Nutrientscape_GAM1_Clamped.tif"))

# apply clamping to GAM2
nutrientscape_GAM2_clamped =
  raster::calc(nutrientscape_GAM2, 
               fun = function(x) clamp_values(x, min_val = 0, max_val = 30))

# save the results
writeRaster(nutrientscape_GAM2_clamped, overwrite = TRUE, 
            here("Data", "Model_Data", "Nutrientscape_GAM2_Clamped.tif"))
