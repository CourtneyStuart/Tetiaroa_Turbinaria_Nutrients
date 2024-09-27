#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "tidyverse", "ggplot2", "PNWColors",
#                   "readxl", "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
#                   "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats", "DHARMa", "see",
#                   "gbm", "spdep", "gstat", "here", "gratia", "raster", "visibly",
#                   "parallel", "doParallel"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
          "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats",
          "DHARMa", "see", "gbm", "spdep", "gstat", "here", "gratia",
          "raster", "visibly", "parallel", "terra", "doParallel")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("relocate", "dplyr")
conflicted::conflict_prefer("partial", "pdp")
conflicted::conflict_prefer("draw", "gratia")
rasterOptions(progress = 'text') # progress info for processing large rasters
rasterOptions(tmpdir = "E:/temp_raster_directory") # custom directory for temporary files
terraOptions(tempdir = "E:/temp_raster_directory") # custom directory for temporary files


#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch1/")
# set_here("F:/Data/StuartC_DPhil_Ch1/") set first-time only
here::i_am(".here")
here::here() # check where we are according to the here package

# save PROJ.4 string for Teti'aroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# read in the rasters that represent our variables (up to 400m offshore)
seabird_biomass = rast(here("Data", "Rasters", "Seabird_Biomass_400m.tif"))
depth = rast(here("Data", "Rasters", "Depth_400m.tif"))
slope = rast(here("Data", "Rasters", "Slope_400m.tif"))
meancurv = rast(here("Data", "Rasters", "QMeanCurve_400m.tif"))
plancurv = rast(here("Data", "Rasters", "QPlanCurve_400m.tif"))
profcurv = rast(here("Data", "Rasters", "QProfCurve_400m.tif"))
rugosity = rast(here("Data", "Rasters", "SAPARugosity_400m.tif"))
eastness = rast(here("Data", "Rasters", "QEastness_400m.tif"))
northness = rast(here("Data", "Rasters", "QNorthness_400m.tif"))
landdist = rast(here("Data", "Rasters", "LandDist_400m.tif"))
fine_habitat = rast(here("Data", "Rasters", "Habitat_400m.tif"))

# we won't include the terrestrial areas when calculating ranges for our 
# environmental variables. remove land from the depth raster.
motus = st_read(here("Data", "GIS", "Motus.shp"))
compareCRS(motus, my_crs)
depth_clipped = mask(depth, motus, inverse = TRUE)
#plot(depth_clipped)

# do the same for all other rasters
rasters_list = list(
  slope = slope,
  meancurv = meancurv,
  plancurv = plancurv,
  profcurv = profcurv,
  eastness = eastness,
  northness = northness,
  rugosity = rugosity,
  landdist = landdist,
  fine_habitat = fine_habitat,
  seabird_biomass = seabird_biomass)
names(rasters_list)

# loop through each raster in the list
for (name in names(rasters_list)) {
  # get the current raster
  current_raster <- rasters_list[[name]]
  
  # clip the raster using depth_clipped as the mask
  clipped_raster <- current_raster
  clipped_raster[is.na(depth_clipped)] <- NA
  
  # create a new variable name with suffix "_clipped"
  new_name <- paste0(name, "_clipped")
  
  # save the clipped raster to the environment
  assign(new_name, clipped_raster, envir = .GlobalEnv)
  
  # run garbage collection to free up memory
  gc()
}

round(minmax(seabird_biomass_clipped, compute = TRUE), digits = 2)
round(minmax(depth_clipped, compute = TRUE), digits = 2)
round(minmax(slope_clipped, compute = TRUE), digits = 2)
round(minmax(meancurv_clipped, compute = TRUE), digits = 2)
round(minmax(profcurv_clipped, compute = TRUE), digits = 2)
round(minmax(plancurv_clipped, compute = TRUE), digits = 2)
round(minmax(eastness_clipped, compute = TRUE), digits = 2)
round(minmax(northness_clipped, compute = TRUE), digits = 2)
round(minmax(rugosity_clipped, compute = TRUE), digits = 2)
round(minmax(landdist_clipped, compute = TRUE), digits = 2)
