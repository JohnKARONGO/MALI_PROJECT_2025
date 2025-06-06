library(tidyverse)
library(janitor)
library(ggridges)
library(camcorder)

gg_record(dir = "yield_march-figures", device = "png", width = 9, height = 8, units = "in", dpi = 320)

freq_df <- read_csv("mali_revised_April_2025.csv") %>% 
  clean_names()

# Define the target columns
target_columns <- c(
  "evi_landsat", "evi_modis", "evi_sentinel",
  "ndvi_landsat", "ndvi_modis", "ndvi_sentinel",
  "ndmi_landsat", "ndmi_modis", "ndmi_sentinel"
)

# Step 1: Filter rows where values in the target columns are between -1 and 1
filtered_df <- freq_df %>%
  filter(if_all(all_of(target_columns), ~ .x >= -1 & .x <= 1))

# Step 2: Compute summary statistics
summary_stats <- filtered_df %>%
  select(-year, -region, -ndmi_landsat, -ndmi_modis, -ndmi_sentinel, -point_x, -point_y, -month, -day) %>% 
  summarise(across(
    where(is.numeric),
    list(
      mean = mean,
      sd = sd,
      min = min,
      q25 = ~ quantile(., 0.25),
      median = median,
      q75 = ~ quantile(., 0.75),
      max = max
    ),
    .names = "{.col}_{.fn}"
  ))




print(summary_stats)



# Load necessary libraries
library(ggplot2)
library(ggridges)
library(dplyr)

# Sample dataset (Replace with your actual dataset)
data <- tibble::tibble(
  year = c(2019, 2019, 2019, 2019, 2019, 2019),
  cultivated_land_ha = c(1, 1, 1, 0.25, 0.25, 0.25),
  yield_kg = c(4500, 4500, 4500, 900, 900, 900),
  month = c(6, 6, 6, 6, 6, 6),
  satellite = c("Landsat", "modis", "Sentinel", "Landsat", "modis", "Sentinel")
)

# Convert month numbers to month names
freq_df <- freq_df %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Boxplot of yield vs cultivated land
ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Cultivated Land (ha)", y = "Yield (kg)", title = "Boxplot of Yield vs Cultivated Land") +
  theme_minimal()

# Violin plot of yield vs cultivated land
ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg)) +
  geom_violin(fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_jitter(width = 0.1, color = "red", alpha = 0.5) +
  labs(x = "Cultivated Land (ha)", y = "Yield (kg)", title = "Violin Plot of Yield vs Cultivated Land") +
  theme_minimal() +
  facet_wrap(~ year, ncol = 2)

# Ridge plot of yield across months
ggplot(freq_df %>% filter(region == "MOPTI"), aes(x = yield_kg, y = month_name, fill = factor(year))) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Yield (kg)", y = "Month", title = "Yield Distribution Across Months", fill = "Year") +
  theme_minimal() +
  facet_wrap(~ year, ncol = 2) 

# Ridge plot of yield across months, faceted by year
ggplot(freq_df, aes(x = yield_kg, y = month_name, fill = as.factor(year))) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Yield (kg)", y = "Month", title = "Yield Distribution Across Months", fill = "Year") +
  theme_minimal() +
  facet_wrap(~ year, ncol = 2) 


# Define the variables for the plot
landsat_NDVI <- freq_df %>% 
  select(-precip, -tmax, -tmin) %>% 
  pivot_wider(names_from = satellite,
              values_from = c(evi, ndvi, ndmi))

ggcorrplot::ggcorrplot(cor(round(landsat_NDVI %>% select(9:17), 2)), lab = TRUE)

# Create the scatterplot
p <- ggplot(landsat_NDVI, aes(x = evi_modis, y = ndvi_modis)) + 
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c(option = "D", direction = 1) +
  geom_point(alpha = 0.3, color = 'black', size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  theme_minimal()
  # facet_wrap(~Facet_Label, ncol = 4) +
  labs(title = "Scatterplots of NDVI Comparisons",
       x = "ASTER NDVI",
       y = "MODIS NDVI",
       fill = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
  
  ggplot(landsat_NDVI, aes(x = evi_modis, y = ndvi_modis)) + 
    stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    # 
    scale_fill_viridis_c(option = "D", direction = 1) +
    geom_point(alpha = 0.3, color = 'blue', size = 0.5) +  # Changed points to blue
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    theme_minimal(base_family = "Arial") + 
    theme(panel.background = element_rect(fill = "white", color = NA),  # Set background to white
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank())  # Removes grid lines for a cleaner look

