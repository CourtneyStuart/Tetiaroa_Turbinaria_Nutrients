#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
#install.packages(c("easypackages", "conflicted", "tidyverse", "ggplot2", "PNWColors", 
#                   "readxl", "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr", 
#                   ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats", "DHARMa", "see",
#                   "gbm", "spdep", "gstat", "here", "gratia", "raster", "visibly", "patchwork"))
library(easypackages)
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "corrplot", "Cairo", "usdm", "caret", "pdp", "ggpubr",
          "ggpmisc", "rsq", "mgcv", "gamclass", "cowplot", "easystats",
          "DHARMa", "see", "gbm", "spdep", "gstat", "here", "gratia",
          "raster", "visibly", "patchwork")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("partial", "pdp")
conflicted::conflict_prefer("draw", "gratia")
conflicted::conflict_prefer("annotate", "ggplot2")

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch1/")
# set_here("E:/Data/StuartC_DPhil_Ch1/") set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

# load working environment from modelling script
load(here("Code", "Nutrientscape_Modelling.RData"))

#### PRETTIER PDPs ####
# make nicer partial dependence plots for the GAM with all predictors
summary(GAM1) # check the results and p-values

GAM1_depth = draw(GAM1, select = "s(Depth)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Depth)", x = "Depth (m)", title = NULL)
GAM1_depth = GAM1_depth + 
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.1, vjust = 1.5)
plot(GAM1_depth)

GAM1_slope = draw(GAM1, select = "s(Slope)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Slope)", x = "Slope (degrees)", title = NULL)
GAM1_slope = GAM1_slope + 
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.3, vjust = 1.6)
plot(GAM1_slope)

GAM1_prof_curve = draw(GAM1, select = "s(Prof_Curve)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Prof_Curve)", x = expression("Profile curvature (" * m^-1 * ")"), 
       title = NULL)
GAM1_prof_curve = GAM1_prof_curve +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.1, vjust = 1.5)
plot(GAM1_prof_curve)

GAM1_plan_curve = draw(GAM1, select = "s(Plan_Curve)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Plan_Curve)", x = expression("Planform curvature (" * m^-1 * ")"), 
       title = NULL) 
GAM1_plan_curve = GAM1_plan_curve +
  annotate("text", x = Inf, y = Inf, label = "italic(p == 0.04)", 
         parse = TRUE, size = 12 / .pt, color = "gray20", 
         hjust = 1.1, vjust = 1.5)
plot(GAM1_plan_curve)

GAM1_eastness = draw(GAM1, select = "s(Eastness)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Eastness)", x = "Eastness", title = NULL)
GAM1_eastness = GAM1_eastness +
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.3, vjust = 1.6)
plot(GAM1_eastness)

GAM1_northness = draw(GAM1, select = "s(Northness)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Northness)", x = "Northness", title = NULL)
GAM1_northness = GAM1_northness +
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.3, vjust = 1.6)
plot(GAM1_northness)

GAM1_birds = draw(GAM1, select = "s(Seabird_Biomass)") & 
  theme_bw() &
  theme(panel.grid = element_blank()) &
  theme(plot.title = element_blank()) &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Seabird_Biomass)", x = "Breeding seabird biomass (kg/ha)", 
       title = NULL)
GAM1_birds = GAM1_birds +
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.3, vjust = 1.6)
plot(GAM1_birds)

GAM1_land = draw(GAM1, select = "s(Land_Distance)") & 
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) &
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) &
  labs(y = "s(Land_Distance)", x = "Distance to land (m)", 
       title = NULL)
GAM1_land = GAM1_land +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.1, vjust = 1.5)
plot(GAM1_land)

# create a function to deal with scientific notation
format_scientific = function(x) {
  formatted_labels = scales::label_number(scale = 1e-5)(x)
  return(formatted_labels)
}

GAM1_latlong = draw(GAM1, select = "s(Longitude_UTM6S,Latitude_UTM6S)") &
  scale_x_continuous(labels = format_scientific) &  # Apply custom formatting to x-axis
  scale_y_continuous(labels = format_scientific) &
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12, colour = "gray20", hjust = 0.5, vjust = -2.5), 
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  guides(color = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in")),
         size = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in"))) &
  labs(y = expression("Latitude UTM Zone 6S (m x 10"^5*")"), 
       x = expression("Longitude UTM Zone 6S (m x 10"^5*")"), 
       title = expression(italic("p") * " < 0.01")) # Use expression() for italic p
print(GAM1_latlong)

# combine the thin plate regression spline results into a multi-panel plot
GAM1_grid = plot_grid(
  GAM1_birds, GAM1_land, GAM1_depth, GAM1_slope, 
  GAM1_prof_curve, GAM1_plan_curve, GAM1_eastness, GAM1_northness,
  ncol = 4, align = 'v')
plot(GAM1_grid)

# add the 2D Duchon spline results to the previous multi-panel plot
GAM1_combined = plot_grid(
  GAM1_latlong,
  GAM1_grid,
  ncol = 1, rel_heights = c(1.75, 3)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# view the result
plot(GAM1_combined)

# save the results for GAM1
save_plot(
  filename = here("Figures", "Smooths_GAM1.png"),
  plot = GAM1_combined,
  base_height = 12, base_width = 16, dpi = 400)


#### COMPARE SHARED SMOOTH TERMS ####
# in summary.gam the p values are of the null hypothesis of a zero effect of the
# indicated spline (relating to the F statistic in the table produced)
summary.gam(GAM1)
summary.gam(GAM2)

# create a function for extracting the smooth terms from the models
extract_smooth_term = function(model, term_name) {
  # draw smooth term from model
  smooth_plot = draw(model, select = term_name, show = FALSE)
  
  # extract data for the term
  smooth_data = smooth_plot$data
  
  # add model and term information
  smooth_data = smooth_data %>%
    mutate(
      model = deparse(substitute(model)),
      term = term_name)
  
  return(smooth_data)
}

#### Depth ####
# extract data for specific smooth terms from both models
smooths_GAM1_depth = extract_smooth_term(GAM1, "s(Depth)")
smooths_GAM2_depth = extract_smooth_term(GAM2, "s(Depth)")

# modify the dataset to use new labels
smooths_depth_combined = bind_rows(
  smooths_GAM1_depth %>% mutate(color = "GAM1"),
  smooths_GAM2_depth %>% mutate(color = "GAM2"))
rm(smooths_GAM1_depth, smooths_GAM2_depth)

# Create the plot with updated legend labels
depth_smooths = ggplot(smooths_depth_combined, 
                       aes(x = Depth, y = est, color = color)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  labs(x = "Depth (m)",
       y = "s(Depth)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# save the legend object for use in later multi-panel plot
legend = ggplotGrob(depth_smooths)$grobs[[which(sapply(ggplotGrob(depth_smooths)$grobs,
                                                       function(x) x$name) == "guide-box")]]
plot(legend)

# now remove the legend from the depth plot
depth_smooths_without_legend = ggplot(smooths_depth_combined, 
                                      aes(x = Depth, y = est, color = color)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  labs(x = "Depth (m)",
       y = "s(Depth)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add rug plot of depth observations
depth_rug_plot = ggplot(model_data, aes(x = Depth)) +
  geom_rug(sides = "b", alpha = 0.5) +
  theme_void()

# combine smooths plot with rug plot
depth_smooths_with_rug = ggplot() +
  geom_line(data = smooths_depth_combined,
            aes(x = Depth, y = est, color = color)) +
  geom_ribbon(data = smooths_depth_combined, 
              aes(x = Depth, y = est, color = color,
                  ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  geom_rug(data = model_data, aes(x = Depth), sides = "b", alpha = 0.5) +
  labs(x = "Depth (m)",
       y = "s(Depth)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add significance labels with background rectangle
depth_smooths_with_rug = depth_smooths_with_rug +
  geom_rect(aes(xmin = -0.5, xmax = 0.0, ymin = 1.2, ymax = 1.5), 
            fill = "white", alpha = 0.4, color = NA, inherit.aes = FALSE) +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.1, vjust = 1.5) +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "darkcyan", 
           hjust = 1.1, vjust = 2.6)

# print the plot
print(depth_smooths_with_rug)

#### distance to land ####
# extract data for specific smooth terms from both models
smooths_GAM1_land = extract_smooth_term(GAM1, "s(Land_Distance)")
smooths_GAM2_land = extract_smooth_term(GAM2, "s(Land_Distance)")

# combine data for plotting
smooths_land_combined = bind_rows(
  smooths_GAM1_land %>% mutate(color = "GAM1"),
  smooths_GAM2_land %>% mutate(color = "GAM2"))
rm(smooths_GAM1_land, smooths_GAM2_land)

land_smooths = ggplot(smooths_land_combined, 
                       aes(x = Land_Distance, y = est, color = color)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  labs(x = "Distance to land (m)",
       y = "s(Land_Distance)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add rug plot of land distance observations
land_rug_plot = ggplot(model_data, aes(x = Land_Distance)) +
  geom_rug(sides = "b", alpha = 0.5) +
  theme_void()

land_smooths_with_rug = ggplot() +
  geom_line(data = smooths_land_combined,
            aes(x = Land_Distance, y = est, color = color)) +
  geom_ribbon(data = smooths_land_combined, 
              aes(x = Land_Distance, y = est, color = color,
                  ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  geom_rug(data = model_data, aes(x = Land_Distance), sides = "b", alpha = 0.5) +  # Add rug plot with different data
  labs(x = "Distance to land (m)",
       y = "s(Land_Distance)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add significance labels
land_smooths_with_rug = land_smooths_with_rug +
  geom_rect(aes(xmin = -0.5, xmax = 0.0, ymin = 0.9, ymax = 1.5), 
            fill = "white", alpha = 0.4, color = NA, inherit.aes = FALSE) +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.1, vjust = 1.5) +
  annotate("text", x = Inf, y = Inf, label = "italic(p < 0.01)", 
           parse = TRUE, size = 12 / .pt, color = "darkcyan", 
           hjust = 1.1, vjust = 2.6)

# print the plot
print(land_smooths_with_rug)

#### Breeding seabird biomass ####
# extract data for specific smooth terms from both models
smooths_GAM1_bird = extract_smooth_term(GAM1, "s(Seabird_Biomass)")
smooths_GAM2_bird = extract_smooth_term(GAM2, "s(Seabird_Biomass)")

# combine data for plotting
smooths_bird_combined = bind_rows(
  smooths_GAM1_bird %>% mutate(color = "GAM1"),
  smooths_GAM2_bird %>% mutate(color = "GAM2"))
rm(smooths_GAM1_bird, smooths_GAM2_bird)

bird_smooths = ggplot(smooths_bird_combined, 
                      aes(x = Seabird_Biomass, y = est, color = color)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  labs(x = "Breeding seabird biomass (kg/ha)",
       y = "s(Seabird_Biomass)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add rug of seabird biomass observations
bird_rug_plot = ggplot(model_data, aes(x = Seabird_Biomass)) +
  geom_rug(sides = "b", alpha = 0.5) +
  theme_void()

bird_smooths_with_rug = ggplot() +
  geom_line(data = smooths_bird_combined,
            aes(x = Seabird_Biomass, y = est, color = color)) +
  geom_ribbon(data = smooths_bird_combined, 
              aes(x = Seabird_Biomass, y = est, color = color,
                  ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.4, linetype = 0) +
  geom_rug(data = model_data, aes(x = Seabird_Biomass), sides = "b", alpha = 0.5) +  # Add rug plot with different data
  labs(x = "Breeding seabird biomass (kg/ha)",
       y = "s(Seabird_Biomass)",
       color = "Model",
       fill = "Model") +
  scale_color_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  scale_fill_manual(values = c("GAM1" = "gray20", "GAM2" = "darkcyan")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

# add significance labels
bird_smooths_with_rug = bird_smooths_with_rug +
  geom_rect(aes(xmin = 950, xmax = 1100, ymin = 2.6, ymax = 3), 
            fill = "white", alpha = 0.4, color = NA, inherit.aes = FALSE) +
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "gray20", 
           hjust = 1.3, vjust = 1.6) +
  annotate("text", x = Inf, y = Inf, label = "NS", 
           parse = TRUE, size = 12 / .pt, color = "darkcyan", 
           hjust = 1.3, vjust = 2.7)

# print the plot
print(bird_smooths_with_rug)

# combine all smooths across GAM1 and GAM2
all_smooths = plot_grid(bird_smooths_with_rug, land_smooths_with_rug,
                        depth_smooths_with_rug, GAM1_slope, 
                        GAM1_prof_curve, GAM1_plan_curve, 
                        GAM1_eastness, GAM1_northness,
                        ncol = 4, nrow = 2)
plot(all_smooths)

combined = plot_grid(legend, all_smooths, nrow = 2, rel_heights = c(0.1, 1),
                     rel_widths = c(1,1)) +
  theme(plot.margin = unit(c(0, 1, 0, 0.25), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
plot(combined)

# save the combined results
save_plot(
  filename = here("Figures", "Combined_Smooths.png"),
  plot = combined,
  base_height = 8, base_width = 16, dpi = 400)

#### COMPARE SHARED DUSCHON SPLINE ####
# GAM1_latlong = draw(GAM1, select = "s(Longitude_UTM6S,Latitude_UTM6S)") &
#   scale_x_continuous(labels = format_scientific) &  # Apply custom formatting to x-axis
#   scale_y_continuous(labels = format_scientific) &
#   theme_bw() &
#   theme(panel.grid = element_blank(),
#         plot.title = element_text(size = 12, colour = "gray20", hjust = 0.5, vjust = -2.5), 
#         axis.title = element_text(size = 16),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         legend.title = element_text(size = 12),
#         plot.caption = element_blank(),
#         plot.subtitle = element_blank()) &
#   guides(color = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in")),
#          size = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in"))) &
#   labs(y = expression("Latitude UTM Zone 6S (m x 10"^5*")"), 
#        x = expression("Longitude UTM Zone 6S (m x 10"^5*")"), 
#        title = expression(italic("p") * " < 0.01")) # Use expression() for italic p
GAM1_latlong = GAM1_latlong +
  theme(plot.title = element_text(size = 12, face = "bold", colour = "gray20",
                                  hjust = 0.5, vjust = -1.5),
        plot.subtitle = element_text(size = 11, face = "italic"),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 11)) +
  ggtitle("GAM1", subtitle = expression(italic("p") * " < 0.01"))
print(GAM1_latlong)

GAM2_latlong = draw(GAM2, select = "s(Longitude_UTM6S,Latitude_UTM6S)") &
  scale_x_continuous(labels = format_scientific) &  # Apply custom formatting to x-axis
  scale_y_continuous(labels = format_scientific) &
  theme_bw() &
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12, colour = "darkcyan", 
                                  hjust = 0.5, vjust = -2.5),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        plot.caption = element_blank(),
        plot.subtitle = element_blank()) &
  guides(color = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in")),
         size = guide_legend(keyheight = unit(4, "in"), keywidth = unit(1, "in"))) &
  labs(y = expression("Latitude UTM Zone 6S (m x 10"^5*")"),
       x = expression("Longitude UTM Zone 6S (m x 10"^5*")"),
       title = expression(italic("p") * " < 0.01")) # Use expression() for italic p
GAM2_latlong = GAM2_latlong +
  theme(plot.title = element_text(size = 12, face = "bold", colour = "darkcyan", 
                                  hjust = 0.5, vjust = -1.5),
        plot.subtitle = element_text(size = 11, face = "italic"),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 11)) +
  ggtitle("GAM2", subtitle = expression(italic("p") * " < 0.01"))

print(GAM2_latlong)

# put the plots next to each other
duchon = plot_grid(GAM1_latlong, GAM2_latlong,ncol = 2,
                   rel_heights = c(0.1, 1)) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
print(duchon)

# save the combined 2D Duchon splines
save_plot(
  filename = here("Figures", "Combined_2D_Duchon_Splines.png"),
  plot = duchon,
  base_height = 4, base_width = 8.5, dpi = 400)

#### SUMMARY STATS ####
summary(GAM1)
AIC_GAM1
RMSE_GAM1

summary(GAM2)
AIC_GAM2
RMSE_GAM2

# plot the raw N15 data
par(mar = c(2.5,2.5,2.5,2.5))
hist(results$N15)
round(median(results$N15), digits = 2)
round(min(results$N15), digits = 2)
round(max(results$N15), digits = 2)

# compare to latitude and longitude
ggplot(results, aes(x = Longitude_UTM6S, y = Latitude_UTM6S, color = N15)) +
  geom_point(size = 3) +  # adjust the size of the points as needed
  scale_color_gradient(low = "navy", high = "red") +  # color gradient from blue to red
  labs(title = "Map of Algae Points Colored by N15 Values",
       x = "Longitude (UTM6S)",
       y = "Latitude (UTM6S)",
       color = "N15") +  # Legend title
  theme_minimal()  # A clean theme for the plot

# compare the western hald of the atoll to the (north/south)east
western_motu = filter(results, Motu %in% c("Onetahi", "Honuea", "Tiaraunu"))
hist(western_motu$N15)
round(min(western_motu$N15), digits = 2)
round(max(western_motu$N15), digits = 2)
round(median(western_motu$N15), digits = 2)
round((sum(western_motu$N15 <= 6) / nrow(western_motu)*100), digits = 2)
round((sum(western_motu$N15 <= 4) / nrow(western_motu)*100), digits = 2)

north_souteastern_motu = filter(results, Motu %in% c("Ahuroa", "Hīra'a'ānae", "Horoāterā", "Rimatu'u", 
                                                   "Reiono", "Tahuna Iti", "Tahuna Rahi"))
hist(north_souteastern_motu$N15)
round(min(north_souteastern_motu$N15), digits = 2)
round(max(north_souteastern_motu$N15), digits = 2)
round(median(north_souteastern_motu$N15), digits = 2)
round((sum(north_souteastern_motu$N15 >= 10) / nrow(north_souteastern_motu)*100), digits = 2)

# plot the highest two N15 values in black
ggplot(results, aes(x = Longitude_UTM6S, y = Latitude_UTM6S)) +
  geom_point(aes(color = N15), size = 3) +  
  geom_point(data = filter(results, N15 == 16.960000), color = "black", size = 4) +
  geom_point(aes(color = N15), size = 3) +  
  geom_point(data = filter(results, N15 == 16.640000), color = "black", size = 4) +
  scale_color_gradient(low = "blue", high = "red") + 
  labs(title = "Map of Algae Points Colored by N15 Values",
       x = "Longitude (UTM6S)",
       y = "Latitude (UTM6S)",
       color = "N15") +
  theme_minimal()

# compare N15 to depth, plot the top 10% N15 values in red
ggplot(results, aes(x = N15, y = Depth, 
                    color = cut(N15, breaks = c(-Inf, quantile(N15, 0.9, na.rm = TRUE), Inf), 
                                labels = c("Others", "Top 10%")))) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Add a best fit line
  scale_color_manual(values = c("Others" = "black", "Top 10%" = "red")) +
  labs(title = "N15 vs. Depth",
       x = "N15 (‰)",
       y = "Depth (m)",
       color = "N15 Category") +
  theme_minimal()

# compare N15 to distance to land, plot the top 10% N15 values in red
ggplot(results, aes(x = Land_Distance, y = N15, 
                    color = cut(N15, breaks = c(-Inf, quantile(N15, 0.9, na.rm = TRUE), Inf), 
                                labels = c("Others", "Top 10%")))) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Add a best fit line
  scale_color_manual(values = c("Others" = "black", "Top 10%" = "red")) +
  labs(title = "dN15 vs. Distance to land",
       x = "distance to Land (m)",
       y = "dN15 (‰)",
       color = "dN15 Category") +
  theme_minimal()

# compare years
N15_2021 = filter(results, Year == 2021)
N15_2023 = filter(results, Year == 2023)
round(median(N15_2021$N15), digits = 2)
round(min(N15_2021$N15), digits = 2)
round(max(N15_2021$N15), digits = 2)


round(median(N15_2023$N15), digits = 2)
round(min(N15_2023$N15), digits = 2)
round(max(N15_2023$N15), digits = 2)

# compare the distribution of errors around the predicted N15 values from each model
errors = results %>%
  select(Error_N15_GAM1, Error_N15_GAM2) %>%
  pivot_longer(
    cols = starts_with("Error_N15"),
    names_to = "Model",
    names_prefix = "Error_N15_",
    values_to = "N15_Error")

errors_GAM1 = filter(errors, Model == "GAM1")
errors_GAM2 = filter(errors, Model == "GAM2")

error_GAM1_plot =
  ggplot(errors_GAM1, aes(x = N15_Error, fill = Model)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.4,
                 color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("GAM1" = "gray20")) +
  labs(y = "Frequency",
       fill = "Model") +
  xlim(-7.5, 7.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.caption = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.position = "none") 
plot(error_GAM1_plot)

error_GAM2_plot =
  ggplot(errors_GAM2, aes(x = N15_Error, fill = Model)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.4,
                 color = "darkcyan", linewidth = 0.5) +
  scale_fill_manual(values = c("GAM2" = "darkcyan")) +
  labs(x = "Error around predicted δ15N (‰)",
       y = "Frequency",
       fill = "Model") +
  xlim(-7.5, 7.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.caption = element_blank(),
        plot.subtitle = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.position = "none") 
plot(error_GAM2_plot)

# put the plots next to each other
errors_plots = plot_grid(error_GAM1_plot, error_GAM2_plot, nrow = 2,
                   rel_widths = c(1,1), rel_heights = c(1,1)) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
plot(errors_plots)

errors_plots2 = plot_grid(legend, errors_plots, nrow = 2,
                          rel_heights = c(0.1, 1), rel_widths = c(1,1)) +
  theme(plot.margin = unit(c(0, 1, 0, 0.25), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
plot(errors_plots2)

# save the combined errors plot
save_plot(
  filename = here("Figures", "Combined_Error_Histograms.png"),
  plot = errors_plots2,
  base_height = 4, base_width = 6, dpi = 400)

# how often were GAM1 errors within 0.5 of the actual value?
(sum(errors_GAM1$N15_Error >= -0.5 & errors_GAM1$N15_Error <= 0.5) / nrow(errors_GAM1)) * 100

# how often were GAM1 errors within 0.5 of the actual value?
(sum(errors_GAM2$N15_Error >= -0.5 & errors_GAM2$N15_Error <= 0.5) / nrow(errors_GAM2)) * 100

# how often were GAM1 errors within 1 of the actual value?
(sum(errors_GAM1$N15_Error >= -1 & errors_GAM1$N15_Error <= 1) / nrow(errors_GAM1)) * 100

# how often were GAM1 errors within 0.5 of the actual value?
(sum(errors_GAM2$N15_Error >= -1 & errors_GAM2$N15_Error <= 1) / nrow(errors_GAM2)) * 100

# plot depth vs. Error_N15_GAM1 with a best fit line
ggplot(results, aes(x = Depth, y = Error_N15_GAM1)) +
  geom_point(color = "gray20", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Best fit line
  labs(title = "Scatterplot of Depth vs. Error_N15_GAM1",
       x = "Depth (m)",
       y = "Error in δ15N predictions (‰)") +
  theme_minimal()

# plot land_distance vs. Error_N15_GAM1 with a best fit line
ggplot(results, aes(x = Land_Distance, y = Error_N15_GAM1)) +
  geom_point(color = "gray20", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Best fit line
  labs(title = "Scatterplot of Land Distance vs. Error_N15_GAM1",
       x = "Distance to land (m)",
       y = "Error in δ15N predictions (‰)") +
  theme_minimal()

# plot seabird biomass vs. Error_N15_GAM1 with a best fit line
ggplot(results, aes(x = Seabird_Biomass, y = Error_N15_GAM1)) +
  geom_point(color = "gray20", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Best fit line
  labs(title = "Scatterplot of Seabird Biomass vs. Error_N15_GAM1",
       x = "Breeding seabird biomass (kg/ha)",
       y = "Error in δ15N predictions (‰)") +
  theme_minimal()

# plot N15 vs. distance to land by motu
ggplot(results, aes(x = Land_Distance, y = N15, fill = Motu)) +
  geom_point(size = 3, shape = 21, stroke = 0.5, color = "black") + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs( x = "Distance to land (m)",
        y = "δN15 (‰)",
        fill = "Motu") +
  ylim(0, 18) +
  xlim(0, 65) +
  theme_bw() +  # use black and white theme
  theme(panel.grid = element_blank(),         # remove grid lines
        plot.title = element_blank(),         # remove the plot title
        axis.ticks = element_line(color = "black"),  # ensure axis tick marks are black and visible
        axis.ticks.length = unit(0.15, "cm"),  # c the length of the tick marks
        axis.ticks.x = element_line(),        # ensure x-axis ticks are drawn
        axis.ticks.y = element_line(),        # ensure y-axis ticks are drawn
        panel.border = element_rect(color = "black")  # ensure all panels have a border
        ) +
  facet_wrap(~ Motu, scales = "free")  # create separate plots for each Motu with free scales to show all tick marks

# save the plot
save_plot(
  filename = here("Figures", "N15_vs_Land_Distance_by_Motu.png"),
  plot = last_plot(),
  base_height = 5, base_width = 10, dpi = 400)

# plot N15 vs. depth by motu
ggplot(results, aes(x = Depth, y = N15, fill = Motu)) +
  geom_point(size = 3, shape = 21, stroke = 0.5, color = "black") + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(x = "Depth below surface (m)",
       y = "δN15 (‰)",
       fill = "Motu") +
  ylim(0, 18) +
  xlim(-2, 0) +
  theme_bw() +  # use black and white theme
  theme(panel.grid = element_blank(),         # remove grid lines
        plot.title = element_blank(),         # remove the plot title
        axis.ticks = element_line(color = "black"),  # ensure axis tick marks are black and visible
        axis.ticks.length = unit(0.15, "cm"),  # control the length of the tick marks
        axis.ticks.x = element_line(),        # ensure x-axis ticks are drawn
        axis.ticks.y = element_line(),        # ensure y-axis ticks are drawn
        panel.border = element_rect(color = "black")  # ensure all panels have a border
        ) +
  facet_wrap(~ Motu, scales = "free")  # create separate plots for each Motu with free scales to show all tick marks

# save the plot
save_plot(
  filename = here("Figures", "N15_vs_Depth_by_Motu.png"),
  plot = last_plot(),
  base_height = 5, base_width = 10, dpi = 400)