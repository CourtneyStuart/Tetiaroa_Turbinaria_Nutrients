#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
#install.packages(c("easypackages", "conflicted", "tidyverse", "ggplot2", "PNWColors", 
#                   "readxl", "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr", 
#                   ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats", "DHARMa", "see",
#                   "gbm", "spdep", "gstat", "here", "gratia", "raster", "visibly"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
          "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats",
          "DHARMa", "see", "gbm", "spdep", "gstat", "here", "gratia",
          "raster", "visibly")
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

# read in the algae-environment data prepped in 03_Preparing_Algae_Data.R
algae = read.csv(here("Data", "Model_Data",
                      "Cleaned_Tetiaroa_2021_2023_Data.csv"))

#### EXPLORATORY PLOTS ####
ggplot(algae, aes(x = N15, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "N15 (‰)", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Slope, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "Slope", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Plan_Curve, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "Planform curvature (1/m)", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Prof_Curve, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "Profile curvature (1/m)", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Eastness, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "Eastness", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Northness, fill = Purpose)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(x = "Northness", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Land_Distance, y = N15, group = Motu, color = Motu)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Best fit lines for each group (Motu)
  labs(x = "Distance offshore (m)", y = "N15 (‰)") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Land_Distance, y = Slope, color = Motu)) +
  geom_point() +
  labs(x = "Distance offshore (m)", y = "Slope (°)") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Land_Distance, y = Plan_Curve, color = Motu)) +
  geom_point() +
  labs(x = "Distance offshore (m)", y = "Planform curvature (1/m)") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(algae, aes(x = Land_Distance, y = Prof_Curve, color = Motu)) +
  geom_point() +
  labs(x = "Distance offshore (m)", y = "Profile curvature (1/m)") +
  theme_bw() +
  theme(panel.grid = element_blank())

#### CORRELATION/COLLINEARITY CHECK ####
# keep only predictor columns
corrdata = algae %>%
  select(Longitude_UTM6S, Latitude_UTM6S, Coarse_Habitat_ID, 18:29, 
         -Aspect, -Onetahi_Distance)

# check for correlation among the predictors
cormat = cor(corrdata, 
             use = "complete.obs")

# save color palette
palette = pnw_palette("Shuksan2", 200, type = "continuous")

# set plotting margins
par(mar = c(0,0,0,0))

# create and save the full correlation plot
corrplot(cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
Cairo(file = here("Figures", "Full_Correlation_Matrix.png"), 
      bg = "white", type = "png",units = "in", width = 6, height = 5, 
      pointsize = 12,  dpi = 600)
par(mar=c(0,0,0,0))
corrplot(cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
dev.off()

# we could run through the correlation matrix manually to find problematic
# variables...but instead we'll automate the process using the "usdm" package. 
# the vifcor function runs through all pairs of variables in the algae data 
# frame and checks whether any exceed the r threshold we define. if so, it will
# tell us where the problems are.
print(vifcor(corrdata, th = 0.7))

# the vifstep function runs through all pairs of variables in the algae
# data frame and checks whether any exceed the VIF threshold. we define. if so, 
# it willtell us where the problems are.
print(vifstep(corrdata, th = 5))

# SAPA rugosity and slope are correlated, does SAPA rugosity or slope really vary
# that much across algae collection sites?
summary(algae$SAPA_Rugosity)
par(mar = c(2,2,2,2))
hist(algae$SAPA_Rugosity)

summary(algae$Slope)
hist(algae$Slope)

# the majority of algae collection sites have a SAPA ratio of ~1, meaning no 
# meaningful structural complexity. slope does vary some, so let's retain that.

# mean curvature is also correlated with both profile and planform curvatures,
# keep the latter two predictors due to their anticipated effects on benthic flow 
# acceleration (deceleration) and convergence (divergence), respectively. 

# remove the variables Mean_Curve and SAPA_Rugosity.
cormat2 = cor(select(corrdata, -Mean_Curve, -SAPA_Rugosity), 
              use = "complete.obs")

# create and save the restricted correlation plot
corrplot(cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)

Cairo(file = here("Figures", "Restricted_Correlation_Matrix.png"), 
      bg = "white", type = "png", units = "in", width = 6, height = 5, 
      pointsize = 12, dpi = 600)
par(mar=c(0,0,0,0))
corrplot(cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
dev.off()

# there shouldn't be any issues left, but double check
print(vifcor(select(corrdata, -Mean_Curve, -SAPA_Rugosity),
             th = 0.7))
print(vifstep(select(corrdata, -Mean_Curve, -SAPA_Rugosity),
              th = 5))
# no remaining collinearity issues once we remove mean curvature and SAPA

# save the correlation plots side by side
# define the correlation plots
corr_plot1 = function() {
  par(mar = c(0, 0, 0, 0))
  corrplot(cormat, method = "color", col = palette, type = "upper",
           order = "original", addCoef.col = "black", number.cex = 0.50, 
           number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
}

corr_plot2 = function() {
  par(mar = c(0, 0, 0, 0))
  corrplot(cormat2, method = "color", col = palette, type = "upper",
           order = "original", addCoef.col = "black", number.cex = 0.50, 
           number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
}

# combine the plots side by side with labels (A) and (B)
library(gridGraphics)
correlation_plots = plot_grid(
  ggdraw(corr_plot1) + theme(plot.background = element_rect(fill = "white", color = NA),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")),
  ggdraw(corr_plot2) + theme(plot.background = element_rect(fill = "white", color = NA),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")),
  ncol = 2, rel_widths = c(1, 1),
  labels = c("(a)", "(b)"), 
  label_size = 14,  
  label_fontface = "bold",
  label_y = c(0.85, 0.85))
plot(correlation_plots) # take a look

# save the combined plot
ggsave(here("Figures", "Correlation_Plots.png"), correlation_plots,
       width = 12, height = 6, dpi = 400, bg = "white")

#### SPATIAL AUTOCORRELATION CHECK ####
# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

# convert the tabuluar algae data to spatial data
algae_sp = st_as_sf(algae,
                 coords = c("Longitude_UTM6S", "Latitude_UTM6S"), 
                 crs = my_crs)
algae_sp = as(algae_sp, "Spatial")

# create a neighbors list based on distance (max search distance of 500m)
nb = dnearneigh(coordinates(algae_sp), d1 = 0, d2 = 500, longlat = FALSE)

# convert neighbors list to a spatial weights matrix
W = nb2listw(nb, style = "W", zero.policy = TRUE)

# visualize relationship between neighbor N15 values
moran.plot(algae_sp$N15, W, zero.policy = TRUE)

# Moran I test under randomisation
moran.test(algae_sp$N15, listw = W, zero.policy = TRUE, randomisation = TRUE)

# Moran I test under normality
moran.test(algae_sp$N15, listw = W, zero.policy = TRUE, randomisation = FALSE)

# Moran I test using 10000 Monte Carlo simulations
moran.mc(algae_sp$N15, listw = W, zero.policy = TRUE, 10000)

# reject the null hypothesis of no spatial autocorrelation in the N15 data. there
# is strong evidence that the data exhibit spatial autocorrelation, specifically
# a pattern of clustering (positive spatial autocorrelation) as indicated by the
# "greater" alternative hypothesis.

# conduct variogram modeling to capture the spatial structure
# empirical variogram 
N15_evgm = variogram(N15 ~ 1, algae_sp)
plot(N15_evgm, xlab = "distance (m)", pch = 19)
# fitted variogram, with some guesses for psill, range, nugget based on the empirical plot
N15_fvgm = fit.variogram(N15_evgm, vgm(psill = 10, model = "Sph", range = 1200, nugget = 2.5)) # fit variogram
print(N15_fvgm)
N15_svgm_plot = plot(N15_evgm, model = N15_fvgm, xlab = "distance (m)", pch = 19)
plot(N15_svgm_plot)
# N15 values demonstrate correlation up to ~1050 m

#### GAMS with LOOCV ####
# update data frame to keep only the columns needed for modelling, 
model_data = algae %>%
  select(N15, Coarse_Habitat_ID, 18:29, 
         Longitude_UTM6S, Latitude_UTM6S, Year,
         -Aspect, -Mean_Curve, -SAPA_Rugosity,
         -Onetahi_Distance) %>%
  mutate(Coarse_Habitat_ID = as.factor(Coarse_Habitat_ID)) %>%
  mutate(Year = as.factor(Year))
str(model_data)

#### GAM1 - All predictors ####
# initialize vectors for storing results
n = nrow(model_data)
GAM1_squared_error = numeric(n)

# add placeholder columns to model_data
model_data$Predicted_N15_GAM1 = NA
model_data$Index_GAM1 = NA

# perform LOOCV
for (i in 1:n) {
  # leave one out
  train_data = model_data[-i, ]
  test_data = model_data[i, , drop = FALSE]
  
  # fit the GAM on the training data
  GAM1 = gam(N15 ~ Coarse_Habitat_ID + s(Seabird_Biomass) + s(Depth) +
               s(Slope) + s(Eastness) + s(Northness) + s(Plan_Curve) + 
               s(Prof_Curve) + s(Land_Distance)  + Year +
               s(Longitude_UTM6S, Latitude_UTM6S, bs = "ds"),
              data = train_data, method = "REML")
  
  # predict on the testing data
  GAM1_predicted = predict(GAM1, newdata = test_data)
  
  # calculate the squared error
  GAM1_squared_error[i] = (GAM1_predicted - test_data$N15)^2
  
  # save the predicted and observed values directly to model_data
  model_data$Predicted_N15_GAM1[i] = GAM1_predicted
  model_data$Index_GAM1[i] = i
  
  # print progress
  cat("Completed iteration", i, "of", n, "\n")
}

# save the model object
saveRDS(GAM1, here("Data", "Model_Data", "GAM1.rds"))

# calculate the average of the squared errors to get the LOOCV error estimate
mean_GAM1_squared_error = round(mean(GAM1_squared_error), dig = 2)

# calculate the root mean squared error (RMSE)
RMSE_GAM1 = round(sqrt(mean(GAM1_squared_error)), dig = 2)
cat("Root Mean Squared Error (RMSE):", RMSE_GAM1, "\n") # print RMSE

# in summary.gam the p values are of the null hypothesis of a zero effect of the
# indicated spline (relating to the F statistic in the table produced)
summary.gam(GAM1)

# in gam.check the p values are for the test of the null hypothesis that the basis
# dimension used is of sufficient size (i.e., these p values relate to the value labelled
# k-index in the table produced by gam.check)
gam.check(GAM1)

# check results vs. model assumptions
diagnostics_GAM1 = plot_gam_check(GAM1, scatter = TRUE)
ggsave(here("Figures", "Diagnostic_Plots_GAM1.png"), diagnostics_GAM1,
       width = 8, height = 6, dpi = 400, bg = "white")
check_model(GAM1, check = c("normality", "homogeneity")) 

# check AIC of the model
AIC_GAM1 = round(AIC(GAM1), dig = 2)

# create partial dependence plots
# base r plotting method from mgcv 
par(mar = c(2,2,1,1))
plot.gam(GAM1, pages = 1)

# prettier plots using ggplot2 and draw from the gratia package
draw(GAM1, residuals = TRUE, rug = TRUE) &
  theme_bw() & theme(panel.grid = element_blank()) &
  theme(plot.title = element_text(size = 8)) &
  theme(axis.title = element_text(size = 8))

# check for remaining spatial autocorrelation in the model residuals
# extract residuals
res_GAM1 = residuals(GAM1) 
# add their coordinate info
coord_GAM1 = cbind(train_data$Longitude_UTM6S, train_data$Latitude_UTM6S)
# create the neighbors list and spatial weights matrix
nb_GAM1 = dnearneigh(coord_GAM1, d1 = 0, d2 = 500, longlat = FALSE)
W_GAM1 = nb2listw(nb_GAM1, style = "W", zero.policy = TRUE)
# Moran I test under randomisation
moran.test(res_GAM1, listw = W_GAM1, zero.policy = TRUE, randomisation = TRUE)
# Moran I test under normality
moran.test(res_GAM1, listw = W_GAM1, zero.policy = TRUE, randomisation = FALSE)
# Moran I test using 10000 Monte Carlo simulations
moran.mc(res_GAM1, listw = W_GAM1, zero.policy = TRUE, 10000)
# plot the results
GAM1_plot = train_data
GAM1_plot$res_GAM1 = res_GAM1
coordinates(GAM1_plot) = ~ Longitude_UTM6S + Latitude_UTM6S
plot(variogram(res_GAM1 ~ 1, data = GAM1_plot))

# good - no evidence of  remaining spatial autocorrelation in the model residuals

#### GAM2 - Land distance, depth, seabirds, lat*long, year ####
GAM2_squared_error = numeric(n)

# add placeholder columns to model_data
model_data$Predicted_N15_GAM2 = NA
model_data$Index_GAM2 = NA

# perform LOOCV
for (i in 1:n) {
  # leave one out
  train_data = model_data[-i, ]
  test_data = model_data[i, , drop = FALSE]
  
  # fit the GAM on the training data
  GAM2 = gam(N15 ~ s(Seabird_Biomass) + s(Depth) + s(Land_Distance) +
                Year + s(Longitude_UTM6S, Latitude_UTM6S, bs = "ds"),
              data = train_data, method = "REML")
  
  # predict on the testing data
  GAM2_predicted = predict(GAM2, newdata = test_data)
  
  # calculate the squared error
  GAM2_squared_error[i] = (GAM2_predicted - test_data$N15)^2
  
  # save the predicted and observed values directly to model_data
  model_data$Predicted_N15_GAM2[i] = GAM2_predicted
  model_data$Index_GAM2[i] = i
  
  # print progress
  cat("Completed iteration", i, "of", n, "\n")
}

# save the model object
saveRDS(GAM2, here("Data", "Model_Data", "GAM2.rds"))

# calculate the average of the squared errors to get the LOOCV error estimate
mean_GAM2_squared_error = round(mean(GAM2_squared_error), dig = 2)

# calculate the root mean squared error (RMSE)
RMSE_GAM2 = round(sqrt(mean(GAM2_squared_error)), dig = 2)
cat("Root Mean Squared Error (RMSE):", RMSE_GAM2, "\n") # print RMSE

# in summary.gam the p values are of the null hypothesis of a zero effect of the
# indicated spline (relating to the F statistic in the table produced)
summary.gam(GAM2)

# in gam.check the p values are for the test of the null hypothesis that the basis
# dimension used is of sufficient size (i.e., these p values relate to the value labelled
# k-index in the table produced by gam.check)
gam.check(GAM2)

# check results vs. model assumptions
diagnostics_GAM2 = plot_gam_check(GAM2, scatter = TRUE)
ggsave(here("Figures", "Diagnostic_Plots_GAM2.png"), diagnostics_GAM2,
       width = 8, height = 6, dpi = 400, bg = "white")
check_model(GAM2, check = c("normality", "homogeneity"))  

# check AIC of the model
AIC_GAM2 = round(AIC(GAM2), dig = 2)

# create partial dependence plots
# base r plotting method from mgcv 
plot.gam(GAM2, pages = 1)

# prettier plots using ggplot2 and draw from gratia package
draw(GAM2, residuals = TRUE, rug = TRUE) &
  theme_bw() & theme(panel.grid = element_blank()) &
  theme(plot.title = element_text(size = 8)) &
  theme(axis.title = element_text(size = 8))

# check for remaining spatial autocorrelation in the model residuals
# extract residuals
res_GAM2 = residuals(GAM2)
# add their coordinate info
coord_GAM2 = cbind(train_data$Longitude_UTM6S, train_data$Latitude_UTM6S)
# create the neighbors list and spatial weights matrix
nb_GAM2 = dnearneigh(coord_GAM2, d1 = 0, d2 = 500, longlat = FALSE)
W_GAM2 = nb2listw(nb_GAM2, style = "W", zero.policy = TRUE)
# Moran I test under randomisation
moran.test(res_GAM2, listw = W_GAM2, zero.policy = TRUE, randomisation = TRUE)
# Moran I test under normality
moran.test(res_GAM2, listw = W_GAM2, zero.policy = TRUE, randomisation = FALSE)
# Moran I test using 10000 Monte Carlo simulations
moran.mc(res_GAM2, listw = W_GAM2, zero.policy = TRUE, 10000)
# plot the results
GAM2_plot = train_data
GAM2_plot$res_GAM2 = res_GAM2
coordinates(GAM2_plot) = ~ Longitude_UTM6S + Latitude_UTM6S
plot(variogram(res_GAM2 ~ 1, data = GAM2_plot))
# good - no evidence of  remaining spatial autocorrelation in the model residuals

#### COMBINE GAM1 & GAM2 RESULTS ####
# combine the model results and calculate errors around predicted N15 values
results = inner_join((algae %>% 
                        mutate(Year = as.factor(Year),
                               Fine_Habitat = as.factor(Fine_Habitat),
                               Fine_Habitat_ID = as.factor(Fine_Habitat_ID),
                               Coarse_Habitat = as.factor(Coarse_Habitat),
                               Coarse_Habitat_ID = as.factor(Coarse_Habitat_ID))),
                     model_data) %>%
  mutate(Error_N15_GAM1 =  Predicted_N15_GAM1 - N15,
         Error_N15_GAM2 = Predicted_N15_GAM2 - N15) %>%
  relocate(Error_N15_GAM1, .after = Predicted_N15_GAM1) %>%
  relocate(Error_N15_GAM2, .after = Predicted_N15_GAM2)
str(results)

# save the model results
write.csv(results, here("Data", "Model_Data", "GAM_Results_Tetiaroa_2021_2023.csv"),
          row.names = FALSE)

# save the working environment
save.image(here("Code", "04_Modelling_Nutrientscape.RData"))

#### SGAM_ns -  full non-spatial GAM for reference ####
# as a test, fit the full GAM1 again, but without including the 2D spatial smoother
# of geographic coordinates. will this result in spatial autocorrelation in the model
# residuals and thus inaccurate results?
sgam_ns_squared_error = numeric(n)

# add placeholder columns to model_data
model_data$Predicted_N15_sgam_ns = NA
model_data$Index_sgam_ns = NA

# perform LOOCV
for (i in 1:n) {
  # leave one out
  train_data = model_data[-i, ]
  test_data = model_data[i, , drop = FALSE]
  
  # fit the GAM on the training data
  sgam_ns = gam(N15 ~ Coarse_Habitat_ID + s(Seabird_Biomass) + s(Depth) +
                  s(Slope) + s(Eastness) + s(Northness) + s(Plan_Curve) + 
                  s(Prof_Curve) + s(Land_Distance)  + Year,
                data = train_data, method = "REML")
  
  # predict on the testing data
  sgam_ns_predicted = predict(sgam_ns, newdata = test_data)
  
  # calculate the squared error
  sgam_ns_squared_error[i] = (sgam_ns_predicted - test_data$N15)^2
  
  # save the predicted and observed values directly to model_data
  model_data$Predicted_N15_sgam_ns[i] = sgam_ns_predicted
  model_data$Index_sgam_ns[i] = i
  
  # print progress
  cat("Completed iteration", i, "of", n, "\n")
}

# calculate the average of the squared errors to get the LOOCV error estimate
mean_sgam_ns_squared_error = round(mean(sgam_ns_squared_error), dig = 2)

# calculate the root mean squared error (RMSE)
RMSE_sgam_ns = round(sqrt(mean(sgam_ns_squared_error)), dig = 2)
cat("Root Mean Squared Error (RMSE):", RMSE_sgam_ns, "\n") # print RMSE

# in summary.gam the p values are of the null hypothesis of a zero effect of the
# indicated spline (relating to the F statistic in the table produced)
summary.gam(sgam_ns)

# in gam.check the p values are for the test of the null hypothesis that the basis
# dimension used is of sufficient size (i.e., these p values relate to the value labelled
# k-index in the table produced by gam.check)
gam.check(sgam_ns)

# check results vs. model assumptions
plot_gam_check(sgam_ns, scatter = TRUE)
check_model(sgam_ns, check = c("normality", "homogeneity"))  


# check AIC of the model
AIC_sgam_ns = round(AIC(sgam_ns), dig = 2)

# create partial dependence plots
# base r plotting method from mgcv 
par(mar = c(2,2,1,1))
plot.gam(sgam_ns, pages = 1)

# prettier plots using ggplot2 and draw from gratia package
draw(sgam_ns, residuals = TRUE, rug = TRUE) &
  theme_bw() & theme(panel.grid = element_blank()) &
  theme(plot.title = element_text(size = 8)) &
  theme(axis.title = element_text(size = 8))

# check for spatial autocorrelation in the residuals
# extract model residuals
res_sgam_ns = residuals(sgam_ns)
# add their coordinate info
coord_sgam_ns = cbind(train_data$Longitude_UTM6S, train_data$Latitude_UTM6S)
# create the neighbors list and spatial weights matrix
nb_sgam_ns = dnearneigh(coord_sgam_ns, d1 = 0, d2 = 500, longlat = FALSE)
W_sgam_ns = nb2listw(nb_sgam_ns, style = "W", zero.policy = TRUE)
# Moran I test under randomisation
moran.test(res_sgam_ns, listw = W_sgam_ns, zero.policy = TRUE, randomisation = TRUE)
# Moran I test under normality
moran.test(res_sgam_ns, listw = W_sgam_ns, zero.policy = TRUE, randomisation = FALSE)
# Moran I test using 10000 Monte Carlo simulations
moran.mc(res_sgam_ns, listw = W_sgam_ns, zero.policy = TRUE, 10000)
# plot the results
sgam_ns_plot = train_data
sgam_ns_plot$res_sgam_ns = res_sgam_ns
coordinates(sgam_ns_plot) = ~ Longitude_UTM6S + Latitude_UTM6S
plot(variogram(res_sgam_ns ~ 1, data = sgam_ns_plot))

# It's a good thing we did spatial GAMs above - there is evidence of remaining spatial 
# autocorrelation in the residuals of the non-spatial GAM!!!!!!!!