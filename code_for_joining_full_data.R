library(tidyverse)

df <- readxl::read_xlsx("MALI_FINAL_DATASET_VF2025.xlsx")

yield_df <- df %>% select(1:4) %>%  
  drop_na() %>% 
  mutate(Region = case_when(Region == "Tombouctou" ~ "TOMBOUCTOU", 
                            TRUE ~ Region)) %>% 
  arrange(Year, Region)

temp_precip <- df %>% select(5:10)

yield_df %>% count(Year, Region)
#98+209+276+228+171+197+82+51+322

# Landsat

LandSat_Timb1 <- read_csv("Tombouctou1_L8.csv")
LandSat_Timb2 <- read_csv("Tombouctou2_L8.csv")

# Transform the date column and filter for 2018 onwards
df_trans_tomb1 <- LandSat_Timb1 %>%
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12)) %>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

df_trans_tomb2 <- LandSat_Timb2 %>%
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12))%>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

df_trans_joined <- df_trans_tomb1 %>% 
  rbind(df_trans_tomb2)

# Year Region         n
# <dbl> <chr>      <int>
#   1  2019 MOPTI         98
# 2  2019 TOMBOUCTOU   209
# 3  2020 MOPTI        276
# 4  2020 TOMBOUCTOU   399
# 5  2021 MOPTI        197
# 6  2021 TOMBOUCTOU    82
# 7  2022 MOPTI         51
# 8  2022 TOMBOUCTOU   322



# Define the function
sample_yearly_data <- function(data, years, sample_sizes, seed = 123) {
  set.seed(seed)
  
  sampled_data <- lapply(seq_along(years), function(i) {
    data %>%
      filter(year == years[i]) %>%
      sample_n(sample_sizes[i], weight = month)
  })
  
  # Combine all sampled data into one dataframe
  final_sampled_data <- do.call(rbind, sampled_data)
  
  return(final_sampled_data)
}

years <- c(2019, 2020, 2021, 2022)
sample_sizes <- c(209, 399, 82, 322)

sampled_data <- sample_yearly_data(df_trans_joined, years, sample_sizes) %>% 
  mutate(Satelite = "Landsat", 
         Region = "TOMBOUCTOU") %>% 
  rename(EVI_Landsat = EVI,
         NDVI_Landsat = NDVI,
         NDMI_Landsat = NDMI,
         Year = year) 


landsat_mopti <- read_csv("Djene_Mopti_L8.csv") %>%
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12))%>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

years <- c(2019, 2020, 2021, 2022)
sample_sizes_mopti <- c(98, 276, 197, 51)

sampled_data_mopti <- sample_yearly_data(landsat_mopti, years, sample_sizes_mopti) %>% 
  mutate(Satelite = "Landsat", 
         Region = "MOPTI") %>% 
  rename(EVI_Landsat = EVI,
         NDVI_Landsat = NDVI,
         NDMI_Landsat = NDMI,
         Year = year) 
landsat_full <- sampled_data %>% 
  rbind(sampled_data_mopti) %>%
  arrange(Year, Region) %>% 
  select(-Year, -Region)

landsat_yield <- yield_df %>% 
  cbind(landsat_full)


## Modis


modis_tomb1 <- read_csv("Tombouctou1_M8.csv") %>% 
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12)) %>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

modis_tomb2 <- read_csv("Tombouctou2_M8.csv") %>% 
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12)) %>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

df_modis_tomb_joined <- modis_tomb1 %>% 
  rbind(modis_tomb2) %>% 
  filter(POINT_X %in% landsat_yield$POINT_X & POINT_Y %in% landsat_yield$POINT_Y)



## Sampling

years <- c(2019, 2020, 2021, 2022)
sample_sizes <- c(209, 399, 82, 322)


sampled_data_mopti <- sample_yearly_data(df_modis_tomb_joined, years, sample_sizes) %>% 
  mutate(Satelite = "modis", 
         Region = "TOMBOUCTOU") %>% 
  rename(EVI_modis = EVI,
         NDVI_modis = NDVI,
         NDMI_modis = NDMI,
         Year = year) 

modis_mopti <- read_csv("Djene_Mopti_M8.csv") %>%
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12))%>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

years <- c(2019, 2020, 2021, 2022)
sample_sizes_mopti <- c(98, 276, 197, 51)

sampled_data_modis_mopti <- sample_yearly_data(modis_mopti, years, sample_sizes_mopti) %>% 
  mutate(Satelite = "Modis", 
         Region = "MOPTI") %>% 
  rename(EVI_modis = EVI,
         NDVI_modis = NDVI,
         NDMI_modis = NDMI,
         Year = year) 

modis_full <- sampled_data_mopti %>% 
  rbind(sampled_data_modis_mopti) %>%
  arrange(Year, Region, month, day, POINT_X, POINT_Y) %>% 
  select(-Year, -Region, -Satelite, -month, -day, -POINT_X, -POINT_Y)

landsat_modis_yield <- landsat_yield %>% 
  arrange(Year, Region, month, day, POINT_X, POINT_Y) %>% 
  cbind(modis_full)



# Sentinel

sent_tomb1 <- read_csv("Tombouctou1_S2.csv") %>% 
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12)) %>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

sent_tomb2 <- read_csv("Tombouctou2_S2.csv") %>% 
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12)) %>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

df_sent_tomb_joined <- sent_tomb1 %>% 
  rbind(sent_tomb2) %>% 
  filter(POINT_X %in% landsat_yield$POINT_X & POINT_Y %in% landsat_yield$POINT_Y)



## Sampling

years <- c(2019, 2020, 2021, 2022)
sample_sizes <- c(209, 399, 82, 322)

sampled_data_tomb_sent <- sample_yearly_data(df_sent_tomb_joined, years, sample_sizes) %>% 
  mutate(Satelite = "sentinel", 
         Region = "TOMBOUCTOU") %>% 
  rename(EVI_Sentinel = EVI,
         NDVI_Sentinel = NDVI,
         NDMI_Sentinel = NDMI,
         Year = year) 


sent_mopti <- read_csv("Djene_Mopti_S2.csv") %>%
  mutate(
    date = as.character(date),  
    year = as.numeric(str_sub(date, 1, 4)),       
    month = as.numeric(str_sub(date, 5, 6)),      
    day = as.numeric(str_sub(date, 7, 8))       
  ) %>% 
  filter(year >= 2019,
         month %in% c(6, 7, 8, 9, 10, 11, 12))%>% 
  select(POINT_X, POINT_Y, year, month, day, EVI, NDVI, NDMI)

years <- c(2019, 2020, 2021, 2022)
sample_sizes_mopti <- c(98, 276, 197, 51)

sampled_data_sent_mopti <- sample_yearly_data(sent_mopti, years, sample_sizes_mopti) %>% 
  mutate(Satelite = "sentinel", 
         Region = "MOPTI") %>% 
  rename(EVI_Sentinel = EVI,
         NDVI_Sentinel = NDVI,
         NDMI_Sentinel = NDMI,
         Year = year) 

sentinel_full <- sampled_data_tomb_sent %>% 
  rbind(sampled_data_sent_mopti) %>%
  arrange(Year, Region, month, day, POINT_X, POINT_Y) %>% 
  select(-Year, -Region, -Satelite, -month, -day, -POINT_X, -POINT_Y)

sentinel_landsat_modis_yield <- landsat_modis_yield %>% 
  arrange(Year, Region, month, day, POINT_X, POINT_Y) %>% 
  cbind(sentinel_full) %>% 
  select(1:11, 13:18) %>% 
  pivot_longer(cols = c("EVI_Landsat", "NDVI_Landsat", "NDMI_Landsat",
                        "EVI_modis", "NDVI_modis", "NDMI_modis",
                        "EVI_Sentinel", "NDVI_Sentinel", "NDMI_Sentinel"),
               names_to = c("Index", "Satellite"),
               names_sep = "_") %>% 
  pivot_wider(names_from = Index, values_from = value)

sample_temp_precip <- temp_precip %>%
  drop_na() %>% 
  sample_n(4902) %>% 
  rename(Precip_MOPTI="Daily_Precip_Mopti", Tmax_MOPTI = "Daily_Tmax_Mopti",
         Tmin_MOPTI = "Daily_Tmin_Mopti", Precip_TOMBOUCTOU = "Daily_Precip_Timbuk", 
         Tmax_TOMBOUCTOU = "Daily_Tmax_Timbuk", Tmin_TOMBOUCTOU = "Daily_Tmin_Timbuk") %>% 
  mutate(n = row_number()) %>% 
  pivot_longer(cols = c("Precip_MOPTI", "Tmax_MOPTI", "Tmin_MOPTI", 
                        "Precip_TOMBOUCTOU", "Tmax_TOMBOUCTOU", 
                        "Tmin_TOMBOUCTOU"),
               names_to = c("Index", "Region"),
               names_sep = "_") %>% 
  arrange(Region) %>% 
  pivot_wider(names_from = Index, values_from = value) %>% 
  sample_n(4902) %>% 
  select(-Region) 
  
Overall_df <- sentinel_landsat_modis_yield %>% 
  cbind(sample_temp_precip) %>% 
  select(-n)


library(tidyverse)



write_csv(Overall_df, "MALI_FINAL_DATASET_VG2025.csv")  

glimpse(Overall_df)

# Filters data from 2019 onward (June–December) to 2022.

# Selects key variables (EVI, NDVI, NDMI).

# Merges the two datasets.

# 4. Sampling Landsat Data

# Uses predefined sample sizes for each year.
# Year Region         n
#   1  2019 MOPTI         98
# 2  2019 TOMBOUCTOU   209
# 3  2020 MOPTI        276
# 4  2020 TOMBOUCTOU   399
# 5  2021 MOPTI        197
# 6  2021 TOMBOUCTOU    82
# 7  2022 MOPTI         51
# 8  2022 TOMBOUCTOU   322


# 5. Process MODIS Data
# Filters relevant months (June–December, 2019+).
# 6. Sampling MODIS Data - Self declared yield
# Ensures MODIS data points match Landsat yield dataset.
# filter(POINT_X %in% landsat_yield$POINT_X & POINT_Y %in% landsat_yield$POINT_Y)

# Ensures chronological and spatial consistency.



