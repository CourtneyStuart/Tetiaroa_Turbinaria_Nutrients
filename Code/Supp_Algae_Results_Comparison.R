#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "dplyr", "tidyr", "ggplot2", "here", "readxl")
library(easypackages)
libraries("easypackages", "dplyr", "tidyr", "ggplot2", "here", "readxl")

#### DIRECTORIES ####
# working directory and relative folder path
setwd("F:/Data/StuartC_DPhil_Ch1/")
#set_here("F:/Data/StuartC_DPhil_Ch1/") #set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

# read in the 2021 sample results from Lancaster University (Lancaster, UK)
lanc = read.csv("C:/Users/court/Downloads/Lanc_StuartC_Tetiaroa_2023_Algae_Resampling.csv")

# read in the 2023 sample results from the British Geological Survey (Nottingham, UK)
bgs = read.csv("C:/Users/court/Downloads/BGS_StuartC_Tetiaroa_2023_Algae_Resampling.csv")

# clean up the data
bgs = bgs %>%
  rename(N15 = X15N,
         C13 = X13C,
         N_percent = N.,
         C_percent = C.) %>%
  select(Sample, N15, N_percent, C13, C_percent, Laboratory)

lanc = lanc %>%
  rename(Sample = Sample.ID,
         N15 = X15N,
         C13 = X13C,
         N_percent = N.,
         C_percent = C.) %>%
  mutate(Laboratory = "Lancaster University (Lancaster, UK)") %>%
  select(Sample, N15, N_percent, C13, C_percent, Laboratory)

lanc = lanc %>%
  mutate(Sample = sub("^LEC[0-9]*_", "", Sample),
         Sample = gsub(" ", "", Sample),                     
         Sample = gsub("1EE", "lee", Sample),                 
         Sample = gsub("(dup|trip)$", "", Sample)  )

# combine the results into a single dataframe
data = rbind(bgs, lanc)

# plot the relationship between total N% and delta N15 
ggplot(data, aes(x = N_percent, y = N15, color = Laboratory)) +
  geom_point() +
  labs(x = "N%", y = "N15") +
  theme_minimal()

# grouping by lab and converting to wide format
data = data %>%
  group_by(Sample, Laboratory) %>%
  summarise(across(c(N15, C13, N_percent, C_percent), mean, na.rm = TRUE), .groups = 'drop')

# reshape the data to wide format to compare results side by side
data_wide = data %>%
  pivot_wider(names_from = Laboratory, values_from = c(N15, C13, N_percent, C_percent)) %>%
  rename(N15_BGS = `N15_British Geological Survey (Nottingham, UK)`,
         N15_Lanc = `N15_Lancaster University (Lancaster, UK)`,
         N_pct_BGS = `N_percent_British Geological Survey (Nottingham, UK)`,
         N_pct_Lanc = `N_percent_Lancaster University (Lancaster, UK)`,
         C13_BGS = `C13_British Geological Survey (Nottingham, UK)`,
         C13_Lanc = `C13_Lancaster University (Lancaster, UK)`,
         C_pct_BGS = `C_percent_British Geological Survey (Nottingham, UK)`,
         C_pct_Lanc = `C_percent_Lancaster University (Lancaster, UK)`)

# calculate the differences between the labs for each measurement
data_wide = data_wide %>%
  mutate(
    diff_N15 = N15_BGS - N15_Lanc,
    diff_C13 = C13_BGS - C13_Lanc,
    diff_N_pct = N_pct_BGS - N_pct_Lanc,
    diff_C_pct = C_pct_BGS - C_pct_Lanc)

# calculate correlations
cor_N15 = cor(data_wide$N15_BGS, data_wide$N15_Lanc, use = "complete.obs")
cor_C13 = cor(data_wide$C13_BGS, data_wide$C13_Lanc, use = "complete.obs")
cor_N_pct = cor(data_wide$N_pct_BGS, data_wide$N_pct_Lanc, use = "complete.obs")
cor_C_pct = cor(data_wide$C_pct_BGS, data_wide$C_pct_Lanc, use = "complete.obs")

# print correlations
cat("Correlation between N15 results: ", cor_N15, "\n")
cat("Correlation between C13 results: ", cor_C13, "\n")
cat("Correlation between N% results: ", cor_N_pct, "\n")
cat("Correlation between C% results: ", cor_C_pct, "\n")

#### N15 PLOT ####
N15_model = lm(N15_BGS ~ N15_Lanc, data = data_wide)

# extract the coefficients
N15_intercept = coef(N15_model)[1]
N15_slope = coef(N15_model)[2]

# create the regression equation as a string
N15_equation = paste0("y = ", round(N15_slope, 2), "x + ", round(N15_intercept, 2))

# calculate the R-squared value
N15_r_squared = summary(N15_model)$r.squared
N15_r_squared_label = paste0("R² = ", round(N15_r_squared, 3))

# plot with regression line, equation, and R-squared value
N15_plot = ggplot(data_wide %>% filter(!is.na(N15_BGS), !is.na(N15_Lanc)), # remove NAs
                  aes(x = N15_BGS, y = N15_Lanc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
  annotate("text", x = Inf, y = -Inf, label = N15_equation, 
           hjust = 1.1, vjust = -0.5, size = 5, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = N15_r_squared_label,
           hjust = 1.1, vjust = -1.5, size = 5, color = "blue") +
  labs(x = "N15 (British Geological Survey)", y = "N15 (Lancaster University)") +
  theme_bw() +
  theme(panel.grid = element_blank()) 
plot(N15_plot)

# save the plot
ggsave(plot = N15_plot, filename = here("Figures", "Supp_StuartC_N15_Comparison.png"),
       width = 4, height = 4, units = "in", dpi = 300)

#### C13 PLOT ####
C13_model = lm(C13_BGS ~ C13_Lanc, data = data_wide)

# extract the coefficients
C13_intercept = coef(C13_model)[1]
C13_slope = coef(C13_model)[2]

# create the regression equation as a string
C13_equation = paste0("y = ", round(C13_slope, 2), "x + ", round(C13_intercept, 2))

# calculate the R-squared value
C13_r_squared = summary(C13_model)$r.squared
C13_r_squared_label = paste0("R² = ", round(C13_r_squared, 3))

# plot with regression line, equation, and R-squared value
C13_plot = ggplot(data_wide %>% filter(!is.na(C13_BGS), !is.na(C13_Lanc)),
                  aes(x = C13_BGS, y = C13_Lanc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
  annotate("text", x = Inf, y = -Inf, label = C13_equation, 
           hjust = 1.1, vjust = -0.5, size = 5, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = C13_r_squared_label,
           hjust = 1.1, vjust = -1.5, size = 5, color = "blue") +
  labs(x = "C13 (British Geological Survey)", y = "C13 (Lancaster University)") +
  theme_bw() +
  theme(panel.grid = element_blank()) 
plot(C13_plot)

# save the plot
ggsave(plot = C13_plot, filename = here("Figures", "Supp_StuartC_C13_Comparison.png"),
       width = 4, height = 4, units = "in", dpi = 300)

#### N PERCENT PLOT ####
N_pct_model = lm(N_pct_BGS ~ N_pct_Lanc, data = data_wide)

# extract the coefficients
N_pct_intercept = coef(N_pct_model)[1]
N_pct_slope = coef(N_pct_model)[2]

# create the regression equation as a string
N_pct_equation = paste0("y = ", round(N_pct_slope, 2), "x + ", round(N_pct_intercept, 2))

# calculate the R-squared value
N_pct_r_squared = summary(N_pct_model)$r.squared
N_pct_r_squared_label = paste0("R² = ", round(N_pct_r_squared, 3))

# plot with regression line, equation, and R-squared value
N_pct_plot = ggplot(data_wide %>% filter(!is.na(N_pct_BGS), !is.na(N_pct_Lanc)),
                    aes(x = N_pct_BGS, y = N_pct_Lanc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
  annotate("text", x = Inf, y = -Inf, label = N_pct_equation, 
           hjust = 1.1, vjust = -0.5, size = 5, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = N_pct_r_squared_label,
           hjust = 1.1, vjust = -1.5, size = 5, color = "blue") +
  labs(x = "N percent (British Geological Survey)", y = "N percent (Lancaster University)") +
  theme_bw() +
  theme(panel.grid = element_blank()) 
plot(N_pct_plot)

# save the plot
ggsave(plot = N_pct_plot, filename = here("Figures", "Supp_StuartC_N%_Comparison.png"),
       width = 4, height = 4, units = "in", dpi = 300)

#### C PERCENT PLOT ####
C_pct_model = lm(C_pct_BGS ~ C_pct_Lanc, data = data_wide)

# extract the coefficients
C_pct_intercept = coef(C_pct_model)[1]
C_pct_slope = coef(C_pct_model)[2]

# create the regression equation as a string
C_pct_equation = paste0("y = ", round(C_pct_slope, 2), "x + ", round(C_pct_intercept, 2))

# calculate the R-squared value
C_pct_r_squared = summary(C_pct_model)$r.squared
C_pct_r_squared_label = paste0("R² = ", round(C_pct_r_squared, 3))

# plot with regression line, equation, and R-squared value
C_pct_plot = ggplot(data_wide %>% filter(!is.na(C_pct_BGS), !is.na(C_pct_Lanc)),
                    aes(x = C_pct_BGS, y = C_pct_Lanc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
  annotate("text", x = Inf, y = -Inf, label = C_pct_equation, 
           hjust = 1.1, vjust = -0.5, size = 5, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = C_pct_r_squared_label,
           hjust = 1.1, vjust = -1.5, size = 5, color = "blue") +
  labs(x = "C percent (British Geological Survey)", y = "C percent (Lancaster University)") +
  theme_bw() +
  theme(panel.grid = element_blank()) 
plot(C_pct_plot)

# save the plot
ggsave(plot = C_pct_plot, filename = here("Figures", "Supp_StuartC_C%_Comparison.png"),
       width = 4, height = 4, units = "in", dpi = 300)

#### t-tests ####
N15_ttest = t.test(data_wide$N15_BGS, data_wide$N15_Lanc)
print(N15_ttest)

C13_ttest = t.test(data_wide$C13_BGS, data_wide$C13_Lanc)
print(C13_ttest)

N_pct_ttest = t.test(data_wide$N_pct_BGS, data_wide$N_pct_Lanc)
print(N_pct_ttest)

C_pct_ttest = t.test(data_wide$C_pct_BGS, data_wide$C_pct_Lanc)
print(C_pct_ttest)
