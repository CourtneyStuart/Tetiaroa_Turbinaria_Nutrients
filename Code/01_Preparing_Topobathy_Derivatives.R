#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### PLEASE NOTE ####
# Due to the fine spatial resolution (30cm by 30cm) and large spatial extent
# (25860 cols x 25740 rows) of the input topographic-bathymetric LiDAR raster,
# this script is both time-intensive and computationally demanding. The full 
# script took several days to run to completion when performed by Courtney
# Stuart on a desktop computer (processor: Intel(R) Xeon(R) Silver 4114 CPU 
# @ 2.20 GHz 2.19 GHzX; installed RAM: 128 GB) connected to a 2TB Seagate 
# external hard drive. Parallel processing may speed-up the calculations, but
# will ultimately depend on the number of processors/cores, amount of RAM, 
# etc. available on your machine/cluster. 

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "raster", "terra", "MultiscaleDTM", "sp",
#                  "sf", "conflicted", "spatialEco", "dplyr", "here", "fasterize",
#                  "nngeo"))

# load all libraries at once with easypackages
library(easypackages)
libraries("raster", "terra", "MultiscaleDTM", "sp", "sf", "conflicted", 
          "spatialEco", "dplyr", "here", "fasterize", "nngeo")
conflict_prefer("terrain", "terra")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
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

#### PROCESSING ####
# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

##### topobathy #####
# read in the 30 cm resolution digital terrain model (DTM) derived from a high-density
# light detection and ranging (LiDAR) dataset generated over Tetiaroa in May 2017. 
# see references:
# Davies N, Field D, Gavaghan D et al (2016) Simulating social-ecological systems: the Island Digital Ecosystem Avatars (IDEA) consortium. GigaScience. pp. 14
# Gruen A, Guo T, Ural S, Troyer M, Kocaman S (2017) DSM/DTM-related investigations of the Moorea Avatar project. Presented at the Asian Conference on Remote Sensing. New Delhi, India, 
# Ural S, Gruen A, Kocaman S (2019) Point clouds over Tetiaroa - 3D modeling of a tropical island by topo-bathymetric lidar. Presented at the ACRS (40th asian conference on remote sensing). 
topobathy = raster(here("Data", "Topobathy", "tetiaroa_dtm_30cm_2017_1.tif"))
compareCRS(topobathy, my_crs) # check projection
res(topobathy) # check resolution, which should be 30cm by 30cm
topobathyspat = rast(topobathy) # convert to SpatRaster for processing/storing

# export topobathy raster to desired project folder
writeRaster(topobathy,
            here("Data", "Rasters", "TopoBathy.tif"),
            overwrite = T)

# derive topographic-bathymetric variables - all calculated using a quadratic 
# local fit and the Queen's case (8 neighboring cells). 

##### slope #####
# rate of maximum change in depth measured in degrees
slope = Qfit(r = topobathyspat, 
             w = c(3,3), 
             unit = "degrees",
             metrics = "qslope")

writeRaster(slope, 
            here("Data", "Rasters", "QSlope.tif"),
            overwrite = T)

rm(slope)
gc()

##### aspect ##### 
# surface direction, measured clockwise from north in degrees
aspect = Qfit(r = topobathyspat, 
              w = c(3,3), 
              metrics = "qaspect")

writeRaster(aspect, 
            here("Data", "Rasters", "QAspect.tif"),
            overwrite = T)

rm(aspect)
gc()

##### eastness ####
# relative eastness of the surface, due west (-1) to due east (+1)
eastness = Qfit(r = topobathyspat, 
                w = c(3,3), 
                metrics = "qeastness")

writeRaster(eastness, 
            here("Data", "Rasters", "QEastness.tif"),
            overwrite = T)

rm(eastness)
gc()

##### northness #####
# relative northness of the surface, due south (-1) to due north (+1)
northness = Qfit(r = topobathyspat, 
                 w = c(3,3), 
                 metrics = "qnorthness")

writeRaster(northness, 
            here("Data", "Rasters", "QNorthness.tif"),
            overwrite = T)

rm(northness)
gc()

##### mean curvature #####
# first derivative of slope and second derivative of depth, measured in m^-1
meanc = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "meanc")

writeRaster(meanc, 
            here("Data", "Rasters", "QMeanCurve.tif"),
            overwrite = T)

rm(meanc)
gc()

##### profile curvature #####
# curvature parallel to the direction of maximum slope, measured in m^-1
# upwardly concave (+), linear (0), or upwardly convex (-)
profc = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "profc")

writeRaster(profc, 
            here("Data", "Rasters", "QProfCurve.tif"),
            overwrite = T)

rm(profc)
gc()

##### planform curvature #####
# curvature perpendicular to the direction of maximum slope, measured in m^-1
# laterally convex (+), linear (0), or laterally concave (-)
planc = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "planc")

writeRaster(planc, 
            here("Data", "Rasters", "QPlanCurve.tif"),
            overwrite = T)

rm(planc)
gc()

##### rugosity #####
# surface area to planar area ratio rugosity (slope-corrected)
# surface roughness or complexity, measured as a ratio
sapa = SAPA(r = topobathyspat, 
            w = c(3,3),
            slope_correction = T)

writeRaster(sapa, 
            here("Data", "Rasters", "SAPARugosity.tif"),
            overwrite = T)

rm(sapa)
gc()

##### habitat #####
# read in the remotely sensed map of terrestrial and marine habitat patch types 
# obtained from the Khaled bin Sultan Living Oceans Foundation’s Global Reef Expedition. 
# see: Purkis SJ, Gleason ACR, Purkis CR et al (2019) High-resolution habitat and bathymetry maps for 65,000 sq. km of Earth’s remotest coral reefs. Coral Reefs. pp. 467-488

# according to the metadata, these habitat data were derived from WorldView-02 
# (WV2) satellite imagery collected by DigitalGlobe, Inc. the images have a per 
# pixel spatial resolution of 2m by 2m, so that is the spatial resolution we'll 
# use when converting from vector to raster
habitat = st_read(here("Data", "Habitat", "FPSOTE_habitats_final.shp"))
compareCRS(habitat, my_crs)

# each habitat patch type is associated with an integer habitat code. we'll use 
# this integer code to define habitats when converting from vector to raster.
# create and save a data.frame of habitat class-code pairs.
hab_cod_df = habitat %>%
  st_drop_geometry() %>%
  select(Habitat, Hab_Cod) %>%
  unique()
write.csv(hab_cod_df,
          here("Data", "Habitat", "Habitat_Class_Raster_Codes.csv"), 
          row.names = F)

# use the topobathy raster as a guide to define our spatial extent, but keep the 
# 2m by 2m spatial resolution of the habitat data
topobathy = raster(here("Data", "Rasters", "TopoBathy.tif"))
guide = raster(ext = extent(topobathy), 
               res = c(2,2), 
               crs = my_crs)

# rasterize the habitat data and export the new 2m by 2m habitat raster
habitat_ras = writeRaster((
  fasterize(sf = habitat,
            raster = guide,
            field = "Hab_Cod",
            fun = "max")), 
  file = here("Data", "Rasters", "Habitat_2m.tif"), 
  format = "GTiff", 
  overwrite = T)
compareCRS(habitat_ras, my_crs) # should be TRUE

# later, we will explore whether we need the detailed habitat data or
# whether we can replace these with simpler, coarser habitat classes.
# here are the fine patch types:
FPSOTE_habitats_2m = 
  c("Shallow fore reef slope", "Shallow fore reef terrace", 
    "Coralline algal ridge", "Deep fore reef slope", 
    "Back reef coral framework", "Back reef coral bommies", 
    "Back reef sediment dominated", "Back reef rubble dominated",
    "Back reef pavement", "Rock", "Beach sand", "Coral rubble",
    "Terrestrial vegetation", "Lagoonal pinnacle reefs massive coral dominated",
    "Lagoonal floor barren", "Lagoonal patch reefs", 
    "Lagoonal floor coral bommies", "Lagoonal sediment apron sediment dominated", 
    "Deep lagoonal water", "Urban", "Lagoonal floor macroalgae on sediment",
    "Area not mapped")

# here are our coarse patch types:
StuartC_Coarse_Habitat_Classes = 
  c("Coral reef and hardbottom", "Coral reef and hardbottom",
    "Coral reef and hardbottom", "Coral reef and hardbottom",
    "Coral reef and hardbottom", "Coral reef and hardbottom",
    "Unconsolidated sediment", "Coral reef and hardbottom",
    "Coral reef and hardbottom", "Rock", "Unconsolidated sediment",
    "Coral reef and hardbottom", "Terrestrial vegetation",
    "Coral reef and hardbottom", "Unconsolidated sediment",
    "Coral reef and hardbottom", "Coral reef and hardbottom",
    "Unconsolidated sediment", "Deep lagoonal water", "Urban",
    "Unconsolidated sediment", "Area not mapped")

# combine them and save the data
coarse = as.data.frame(cbind(FPSOTE_habitats_2m, StuartC_Coarse_Habitat_Classes))
write.csv(coarse, 
          here("Data", "Habitat", "StuartC_Coarse_Habitat_Classes.csv"),
          row.names = FALSE)

##### land distance #####
# distance to the nearest point on land, measured in meters
# first use the topobathy data to define land vs. water
# reclassify the raster where values <0.5m  are underwater and values >= 0.5m are 
# above water (land). The 0.5m cut-off is based on the mean tidal range in Tetiaroa 
# according to Jeanson et al. 2014 (https://doi.org/10.2112/SI70-030.1)
landsea = calc(topobathy, fun = function(x) {
  ifelse(is.na(x), 0, ifelse(x >= 0.5, 1, 0))})

# plot the land vs. sea raster to make sure everything looks okay
par(mar = c(0,0,0,0))
plot(landsea)

# save the land vs. sea raster to the temporary folder as we'll use it again later
writeRaster(landsea, here("Temp", "Land_vs_Sea.tif"), 
            format = "GTiff", overwrite = TRUE)

# calculate a new raster where cell values store the distance from each cell to land
land_dist = gridDist(x = rast(landsea), target = 1, overwrite = TRUE)

# crop/mask the land distance raster back to the same extent as the topobathy raster
land_dist = mask(crop(land_dist, topobathyspat), topobathyspat)
plot(land_dist)

# save the result
writeRaster(land_dist, 
            here("Data", "Rasters", "LandDist.tif"),
            overwrite = TRUE)

#### create motu shapefile ####
# define a function for converting rasters to polygons 
# credit: John Baumgartner (johnbaums / polygonize.R on github)
polygonize = function(srcfile, dstfile, mask, ogr_format, fieldname, band, connect8 = FALSE) {
  options = if(isTRUE(connect8)) 'CONNECT8=8' else character(0)
  contour_options = character(0)
  if(missing(mask)) mask = character(0)
  .Call("_sf_CPL_polygonize", PACKAGE = "sf", srcfile, mask, 
        'GTiff', ogr_format, layer = dstfile, options, 0, #iPixValField, 
        contour_options, use_contours = FALSE, use_integer = TRUE)
}

# now convert the land vs. sea raster to simple polygons
polygonize(srcfile = here("Temp", "Land_vs_Sea.tif"), 
           dstfile = here("Temp", "Land_vs_Sea.shp"), 
           ogr_format ='ESRI Shapefile', connect8 = F)

# read back in the polygons that were just created, keeping only the land 
landsea_sf = st_read(here("Temp", "Land_vs_Sea.shp")) %>%
  st_transform(., my_crs) %>%
  filter(Value == 1) # keep only land

# calculate the area of each polygon in square meters
landsea_sf$Area = st_area(landsea_sf)

# keep only the motu (exclude any small sandbars or places where the tops of 
# the reef crest or coral bommies are exposed)
land = landsea_sf %>%
  mutate(Area_sqm = as.numeric(Area)) %>%
  filter(Area_sqm >= 6637.77) # Tahuna Rahi is the smallest motu we want to keep

# add a motu column for names and assign based on the Area_sqm
land = land %>%
  mutate(Motu = case_when(
    abs(Area_sqm - 6637.77) < 1e-2 ~ "Tahuna Rahi",
    abs(Area_sqm - 49336.38) < 1e-2 ~ "Tahuna Iti",
    abs(Area_sqm - 7271.1) < 1e-2 ~ "Tahuna Iti",
    abs(Area_sqm - 220630.32) < 1e-2 ~ "Reiono",
    abs(Area_sqm - 776160.9) < 1e-2 ~ "Rimatuu",
    abs(Area_sqm - 730852.74) < 1e-2 ~ "Onetahi",
    abs(Area_sqm - 288733.68) < 1e-2 ~ "Honuea",
    abs(Area_sqm - 1673405.46) < 1e-2 ~ "Tiaraunu",
    abs(Area_sqm - 59701.32) < 1e-2 ~ "Tauini",
    abs(Area_sqm - 38883.33) < 1e-2 ~ "Auroa",
    abs(Area_sqm - 315116.46) < 1e-2 ~ "Hiraanae",
    abs(Area_sqm - 832803.03) < 1e-2 ~ "Oroatera",
    abs(Area_sqm - 17719.83) < 1e-2 ~ "Aie",
    TRUE ~ NA_character_  # assign NA for areas that do not match any condition
  ))

# ensure the geometries are valid
land = st_make_valid(land)

# separate Tahuna Iti because it has two polygons
tahuna_iti = land %>%
  filter(Motu == "Tahuna Iti")

# dissolve boundaries within each of the remaining polygons to fill interior holes -
# these holes are either lakes/ponds or human-modified land areas that we're not 
# interested in
land_filled = land %>%
  group_by(Motu) %>%
  filter(! Motu == "Tahuna Iti") %>%
  summarize(geometry = st_union(geometry)) %>%
  st_cast("POLYGON")
require(nngeo) 
land_filled = st_remove_holes(land_filled)

# add Tahuna Iti back in with the other motu
tahuna_iti = tahuna_iti %>%
  select(Motu, Area_sqm, geometry)
land_filled = land_filled %>%
  mutate(Area_sqm = as.numeric(st_area(.))) %>%
  select(Motu, Area_sqm, geometry)
motus = rbind(land_filled, tahuna_iti)
compareCRS(motus, my_crs) # should be TRUE
plot(motus) # looks good

# save the motu as a shapefile for mapping and later use
st_write(motus,
         here("Data", "GIS", "Motus.shp"),
         append = FALSE)

##### onetahi distance #####
# calculate a new raster where cell values store the distance from each cell to the
# nearest point along Onetahi's coast
onetahi = motus %>%
  filter(Motu == "Onetahi") %>%
  mutate(gridcode = 1)

# create an empty template raster based on the topobathy raster
template = raster(ext = extent(topobathy), res = res(topobathy), crs = crs(topobathy))
compareRaster(template, topobathy, extent = T, res = T, crs = T, rowcol = T) # should be TRUE

# convert the Onetahi polygon to a raster, where gridcode = 1 for Onetahi
onetahi_rast = (fasterize(onetahi, template, 
                          field = "gridcode", fun = "max"))
gc() # trying to save space

# convert from large raster data layer to a spat raster
onetahi_rast = rast(onetahi_rast)
gc()

# convert NA cells in onetahi_rast to 0 to represent areas that are not Onetahi
onetahi_rast[is.na(onetahi_rast)] = 0
gc()

# now calculate distance from all other cells to Onetahi/value 1
dist_onetahi = gridDist(onetahi_rast, target = 1, overwrite = TRUE)

# clip the raster to the topobathy data to ensure matching extents
dist_onetahi = raster(dist_onetahi)
dist_onetahi_crop = mask(crop(dist_onetahi, extent(topobathy)), topobathy)
compareRaster(dist_onetahi_crop, topobathy, extent = T, crs = T, res = T, rowcol = T) # should be TRUE
# plot the result
par(mar = c(0, 0, 0, 0))
plot(dist_onetahi)

# save the result
writeRaster(dist_onetahi_crop, 
            here("Data", "Rasters", "OnetahiDist.tif"),
            overwrite = T)

#### OPTIONAL CHECKS ####
# read back in all rasters and check CRS, extent, res, and rowcol
# topobathy = raster(here("Data", "Rasters", "TopoBathy.tif"))
# slope = raster(here("Data", "Rasters", "QSlope.tif"))
# aspect = raster(here("Data", "Rasters", "QAspect.tif"))
# eastness = raster(here("Data", "Rasters", "QEastness.tif"))
# northness = raster(here("Data", "Rasters", "QNorthness.tif"))
# profc = slope = raster(here("Data", "Rasters", "QProfCurve.tif"))
# planc = raster(here("Data", "Rasters", "QPlanCurve.tif"))
# meanc = raster(here("Data", "Rasters", "QMeanCurve.tif"))
# sapa = raster(here("Data", "Rasters", "SAPARugosity.tif"))
# habitat = raster(here("Data", "Rasters", "Habitat_2m.tif"))
# landdist = raster(here("Data", "Rasters", "LandDist.tif"))
# onetahidist = raster(here("Data", "Rasters", "OnetahiDist.tif"))

# the next lines should return TRUE
# compareRaster(x = c(topobathy, slope, aspect, eastness, northness, profc, planc, meanc, sapa,
#                 landdist, onetahidist), extent = T, crs = T, rowcol = T, res = T)

# remember that habitat has a 2 x 2 m resolution at this point, so only check the CRS
#compareCRS(habitat, topobathy)
