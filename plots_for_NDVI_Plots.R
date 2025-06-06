library(tidyverse)
library(camcorder)

gg_record(dir = "yield_march-figures/descriptives", device = "png", width = 9, height = 8, units = "in", dpi = 320)

landsat_mopti <- read_csv("Reduced Sample Points (1)/Reduced Sample Points/Landsat 8/Djene_Mopti_L8.csv")

modis_mopti <- read_csv("Reduced Sample Points (1)/Reduced Sample Points/MODIS/Djene_Mopti_M8.csv")

sentinel <- read_csv("Reduced Sample Points (1)/Reduced Sample Points/Sentinel 2/Djene_Mopti_S2.csv")

# Process the dataset
landsat_mopti <- landsat_mopti %>%
  mutate(
    date = as.Date(as.character(date), format = "%Y%m%d"),  # Convert numeric date to Date format
    Year = factor(year(date)),  
    Month = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb)
  ) %>%
  group_by(Year, Month) %>%
  summarise(
    Avg_NDVI = mean(NDVI, na.rm = TRUE),
    Day = min(date, na.rm = TRUE),  # Reference day for plotting
    .groups = "drop"
  )

# Find midpoints for each year for positioning labels inside the plot
year_midpoints <- landsat_mopti %>%
  group_by(Year) %>%
  summarise(Midpoint = mean(Day, na.rm = TRUE), .groups = "drop")

# Plot using geom_line()
ggplot(landsat_mopti, aes(x = Day, y = Avg_NDVI, group = Year, color = Year)) +
  geom_line(size = 1.2) +
  scale_y_continuous(name = "NDVI", limits = c(0, 0.8)) +
  scale_x_date(breaks = landsat_mopti$Day, labels = format(landsat_mopti$Day, "%b"), expand = c(0, 0)) +
  labs(title = "Monthly Variation of NDVI", x = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Add year labels inside the plot at the midpoints
  geom_text(data = year_midpoints, aes(x = Midpoint, y = 0.75, label = Year),
            size = 5, fontface = "bold", color = "darkred")


# Load required libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(purrr)

# Define file paths (update these with actual file paths)
file_paths <- list(
  "Djene_Mopti_L8.csv",
  "Djene_Mopti_M8.csv",
  "Djene_Mopti_S2.csv"
)

# Define a function to process and plot each dataset
process_and_plot <- function(file_path) {
  # Read data
  df <- read.csv(file_path)
  
  # Convert date and extract Year and Month
  df <- df %>%
    mutate(
      date = as.Date(as.character(date), format = "%Y%m%d"),  # Convert numeric date to Date format
      Year = year(date),  
      Month = month(date)  # Convert Month to numeric (1 for Jan, 2 for Feb, etc.)
    ) %>%
    filter(Year >= 2019 & Year <= 2022) %>%  # Filter for 2019-2022
    mutate(Year = factor(Year)) %>%  # Convert Year to factor after filtering
    group_by(Year, Month) %>%
    summarise(
      Avg_NDVI = mean(NDVI, na.rm = TRUE),
      Day = min(date, na.rm = TRUE),  # Reference day for plotting
      .groups = "drop"
    )
  
  # Find midpoints for each year for positioning labels inside the plot
  year_midpoints <- df %>%
    group_by(Year) %>%
    summarise(Midpoint = mean(Day, na.rm = TRUE), .groups = "drop")
  
  # Extract dataset name from file path for labeling
  dataset_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Determine the x-position of August (Month 8)
  august_x <- df$Day[df$Month == 8][1]  # Select the first occurrence of August
  
  # Generate the plot
  p <- ggplot(df, aes(x = Day, y = Avg_NDVI, group = Year, color = Year)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = as.numeric(august_x), linetype = "dotted", color = "black", size = 1) +  # Dotted vertical line for August
    scale_y_continuous(name = "NDVI", limits = c(0, 0.8)) +
    scale_x_date(
      breaks = df$Day,
      labels = df$Month,  # Use month numbers instead of names
      expand = c(0, 0)
    ) +
    labs(title = paste("Monthly Variation of NDVI (2019-2022) -", dataset_name), x = "Month") +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Horizontal x-axis labels
      axis.ticks.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()  # Remove all grid lines
    ) +
    # Add year labels inside the plot at the midpoints
    geom_text(data = year_midpoints, aes(x = Midpoint, y = 0.75, label = Year),
              size = 5, fontface = "bold", color = "darkred")
  
  return(p)
}

# Process and plot for each dataset
plots <- map(file_paths, process_and_plot)

# Print all plots
print(plots[[1]])  # Landsat
print(plots[[2]])  # MODIS
print(plots[[3]])  # Sentinel



# Load required libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(purrr)

# Define file paths (update these with actual file paths)
file_paths <- list(
  "Djene_Mopti_L8.csv",
  "Djene_Mopti_M8.csv",
  "Djene_Mopti_S2.csv"
)

# Define a function to read and process data
read_and_process_data <- function(file_path, source_name) {
  df <- read.csv(file_path) %>%
    mutate(
      date = as.Date(as.character(date), format = "%Y%m%d"),  # Convert numeric date to Date format
      Year = year(date),  
      Month = month(date),  # Convert Month to numeric (1 for Jan, 2 for Feb, etc.)
      Source = source_name   # Add dataset name as a new column
    ) %>%
    filter(Year >= 2019 & Year <= 2022)  # Keep only 2019-2022 data
  
  return(df)
}


# Read and combine all datasets
datasets <- list(
  landsat = read_and_process_data("Djene_Mopti_L8.csv", "Landsat"),
  modis = read_and_process_data("Djene_Mopti_M8.csv", "MODIS"),
  sentinel = read_and_process_data("Djene_Mopti_S2.csv", "Sentinel")
)

# Filter out NULL datasets
datasets <- datasets[!sapply(datasets, is.null)]

# Check if any valid datasets exist
if (length(datasets) == 0) {
  stop("No valid datasets found. Please check file paths and formats.")
}

# Combine all datasets
combined_data <- bind_rows(datasets)

# Ensure NDVI is numeric and remove NA values
combined_data <- combined_data %>%
  filter(!is.na(NDVI)) %>%
  mutate(NDVI = as.numeric(NDVI))

# Plot - Density Plot with Points
ggplot(combined_data, aes(x = NDVI, fill = Source, color = Source)) +
  geom_density(alpha = 0.3, size = 1, na.rm = TRUE) +  # Handle missing values
  geom_point(aes(y = 0), position = position_jitter(width = 0.01), 
             alpha = 0.6, size = 1.2, na.rm = TRUE) +  # Handle missing values
  scale_fill_manual(values = c("Landsat" = "blue", "MODIS" = "red", "Sentinel" = "green")) +
  scale_color_manual(values = c("Landsat" = "blue", "MODIS" = "red", "Sentinel" = "green")) +
  labs(
    title = "NDVI Distribution Across Data Sources (2019-2022)",
    x = "NDVI",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )
