# Load necessary libraries
library(tidyverse)
library(janitor)
library(camcorder)
library(correlation)
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(readxl)
library(ggplot2)

gg_record(dir = "yield_march-figures/correlation_plot", device = "png", width = 9, height = 8, units = "in", dpi = 320)

freq_df <- read_csv("C:/Users/moubi/Downloads/git_files/mali_rice/mali_rice_prediction/mali_revised_April_2025.csv") %>% 
  clean_names()

mopti_freq <- freq_df %>% 
  filter(region == "MOPTI")

tom_freq <- freq_df %>% 
  filter(region == "TOMBOUCTOU")



# Select relevant variables
vars <- c("cultivated_land_ha", "yield_kg_per_ha", "evi_landsat", "evi_modis", "evi_sentinel", "ndvi_landsat", "ndvi_modis", "ndvi_sentinel",      "ndmi_landsat", "ndmi_modis", "ndmi_sentinel", "tmin", "precip", "tmax")
df_selected <- freq_df[vars]
df_selected <- na.omit(df_selected)

# correlation table
corr_table <- correlation::correlation(df_selected,
                                       include_factors = TRUE, method = "auto"
) 

corr_table

ggcorrplot::ggcorrplot(cor(round(df_selected, 2)), lab = TRUE)

# By reion
# Mopti

mopti_df_selected <- mopti_freq[vars]
mopti_df_selected <- na.omit(mopti_df_selected)

ggcorrplot::ggcorrplot(cor(round(mopti_df_selected, 2)), lab = TRUE)

# tombouctou

tom_df_selected <- tom_freq[vars]
tom_df_selected <- na.omit(tom_df_selected)

ggcorrplot::ggcorrplot(cor(round(tom_df_selected, 2)), lab = TRUE)



# to run later
# Load required libraries
library(GGally)
library(ggplot2)


# Generate pairwise plots
ggpairs(freq_df, columns = c(4, 10, 13, 16, 19, 20))
# mopti_plot
ggpairs(mopti, columns = c(9:17))

