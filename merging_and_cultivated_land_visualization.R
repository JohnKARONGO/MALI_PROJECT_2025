library(tidyverse)
library(readxl)
library(splitstackshape)
library(camcorder)

gg_record(dir = "yield_march-figures/descriptive plots1", device = "png", width = 9, height = 8, units = "in", dpi = 320)

pre_merged <- read_csv("MALI_FINAL_DATASET_VG2025.csv") %>% 
  select(-Precip, -Tmax, -Tmin) %>% 
  pivot_wider(names_from = Satellite,
              values_from = c(EVI, NDVI, NDMI))


precip <- read_xlsx("climate_new_march_2025/daily_precip_Mopti.xlsx") %>% 
  filter(year >= 2019, 
         month >= 6) %>% 
  rename(precip = "Djenné")
tmax <- read_xlsx("climate_new_march_2025/daily_Tmax_Mopti.xlsx")%>% 
  filter(year >= 2019, 
         month >= 6) %>% 
  rename(tmax = "Djenné")
tmin <- read_xlsx("climate_new_march_2025/daily_Tmin_Mopti.xlsx")%>% 
  filter(year >= 2019, 
         month >= 6) %>% 
  rename(tmin = "Djenné")

mopti_climate <- tmin %>% 
  left_join(precip) %>% 
  left_join(tmax) %>% 
  drop_na(precip) %>% 
  mutate(Region = "MOPTI")
  # rename(precip_MOPTI = precip, tmax_MOPTI = tmax, tmin_MOPTI = tmin)
  


read_csv("tombouctou_climate_new_march_2025/daily_precip_Timbuktu.csv") %>% 
  filter(year >= 2019 & year<= 2022, 
         month >= 6) -> tom_prec
read_csv("tombouctou_climate_new_march_2025/daily_Tmax_Timbuktu.csv") %>% 
  filter(year >= 2019 & year<= 2022, 
         month >= 6) -> tom_tmax
read_csv("tombouctou_climate_new_march_2025/daily_Tmin_Timbuktu.csv") %>% 
  filter(year >= 2019 & year<= 2022, 
         month >= 6) -> tom_tmin

tom_climate <- tom_prec %>% 
  left_join(tom_tmax) %>% 
  left_join(tom_tmin)%>% 
  mutate(Region = "TOMBOUCTOU") 


climate_df <- mopti_climate %>% 
  rbind(tom_climate)


# Update the sampling to exclude 'day' from the stratification
set.seed(146)
sampled_df <- stratified(pre_merged, 
                         group = c("Year", "Region", "month"), 
                         size = 1712, 
                         replace = FALSE)

# If we have too many rows, randomly sample down to exactly 1712
if(nrow(sampled_df) > 1712) {
  sampled_df <- sampled_df[sample(1:nrow(sampled_df), 1712), ]
}


mali_revised_df <- sampled_df %>% 
  rename(year = Year) %>% 
  inner_join(climate_df) %>% 
  rename(Yield_kg_per_ha = `Yield(Kg)`)


write_csv(mali_revised_df, "mali_revised_April_2025.csv")

mali_revised_df <- read_csv("mali_revised_April_2025.csv")

glimpse(mali_revised_df)

# Compute the summary table
ndvi_summary <- mali_revised_df %>%
  group_by(Region) %>%
  summarise(
    mean_Landsat_NDVI = mean(NDVI_Landsat, na.rm = TRUE),
    sd_Landsat_NDVI = sd(NDVI_Landsat, na.rm = TRUE),
    mean_MODIS_NDVI = mean(NDVI_modis, na.rm = TRUE),
    sd_MODIS_NDVI = sd(NDVI_modis, na.rm = TRUE),
    mean_Sentinel_NDVI = mean(NDVI_Sentinel, na.rm = TRUE),
    sd_Sentinel_NDVI = sd(NDVI_Sentinel, na.rm = TRUE)
  )

# Print the summary table
print(ndvi_summary)

# Boxplot of yield vs cultivated land
ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg_per_ha)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Cultivated Land (ha)", y = "Yield (kg/ha)", title = "Boxplot of Yield vs Cultivated Land") +
  theme_minimal() +
  facet_wrap(~year)

# Boxplot of yield vs cultivated land with multiple years distinguished by color
ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg_per_ha, fill = as.factor(year))) +
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.6) +  # Adjust transparency for better visibility
  labs(x = "Cultivated Land (ha)", y = "Yield (kg/ha)", title = "Boxplot of Yield vs Cultivated Land (Multiple Years)", fill = "Year") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) 


# Boxplot of yield vs cultivated land with multiple years distinguished by color and extended whiskers
plot12 <- ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg_per_ha, fill = as.factor(year))) +
  geom_boxplot(outlier.shape = 16, outlier.size = 0.5, outlier.alpha = 0.7, color = "black", alpha = 0.6) +  # Keep outliers as dots
  stat_summary(fun = max, geom = "errorbar", aes(group = as.factor(year)), 
               width = 0.4, color = "black", linewidth = 1) +  # Adds a bar at the max
  labs(x = "Cultivated Land (ha)", y = "Yield (kg/ha)", 
       title = "Boxplot of Yield vs Cultivated Land \n By year", 
       fill = "Year") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange"))

plot12 <- ggplot(freq_df, aes(x = as.factor(cultivated_land_ha), y = yield_kg_per_ha, fill = as.factor(year))) +
  geom_boxplot(outlier.shape = 16, outlier.size = 0.5, outlier.alpha = 0.7, color = "black", alpha = 0.6) +  # Keep outliers as dots
  stat_summary(fun = max, geom = "errorbar", aes(group = as.factor(year)), 
               width = 0.4, color = "black", linewidth = 1) +  # Adds a bar at the max
  labs(x = "Cultivated Land (ha)", y = "Yield (kg/ha)", 
       title = "Boxplot of Yield vs Cultivated Land \n By year", 
       fill = "Year") +
  theme_classic() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = c(0, 2500, 5000,7500, 10000, 12500, 15000, 17500, 15000)
  )


region_df <- freq_df %>%
  mutate(region = as.factor(region)) %>% 
  drop_na(region)


# Boxplot of yield vs cultivated land with multiple years distinguished by color and extended whiskers
plot13 <- ggplot(region_df , aes(x = as.factor(cultivated_land_ha), y = yield_kg_per_ha, fill = as.factor(region))) +
  geom_boxplot(outlier.shape = 16, outlier.size = 0.5, outlier.alpha = 0.7, color = "black", alpha = 0.6) +  # Keep outliers as dots
  stat_summary(fun = max, geom = "errorbar", aes(group = as.factor(region)), 
               width = 0.4, color = "black", linewidth = 1) +  # Adds a bar at the max
  labs(x = "Cultivated Land (ha)", y = "Yield (kg/ha)", 
       title = "By Region", 
       fill = "Region") +
  theme_classic() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange"))+
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = c(0, 2500, 5000,7500, 10000, 12500, 15000, 17500, 15000)
  )

library(patchwork)

patch1 <- plot12 / plot13

