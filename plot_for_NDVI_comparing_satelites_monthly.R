# Load required libraries
library(ggplot2)
library(lubridate)
library(dplyr)

# Generate example data  
set.seed(123)
dates <- seq(as.Date("2011-01-01"), as.Date("2014-12-31"), by = "day")
n <- length(dates)

# Create seasonal pattern for NDVI  
season_pattern <- sin(2 * pi * (1:n) / 365) * 0.3 + 0.4  

# Generate NDVI data with seasonal patterns and some noise  
data <- data.frame(
  Day = dates,
  GOCI_NDVI = pmax(0, pmin(0.8, season_pattern + rnorm(n, 0, 0.05))),
  MODIS_NDVI = pmax(0, pmin(0.8, season_pattern + rnorm(n, 0, 0.07) - 0.05))
)

# Extract Year and Month for custom x-axis
data$Year <- factor(year(data$Day))  # Convert Year to factor
data$Month <- factor(month(data$Day, label = TRUE, abbr = TRUE), 
                     levels = month.abb)  # Month as ordered factor

# Create a summarized dataset for plotting (Include NDVI averages!)
data_summary <- data %>%
  group_by(Year, Month) %>%
  summarise(Day = min(Day),  
            Avg_GOCI_NDVI = mean(GOCI_NDVI),  
            Avg_MODIS_NDVI = mean(MODIS_NDVI),  
            .groups = "drop")

# Find midpoints for each year (to position the year labels inside the plot)
year_midpoints <- data_summary %>%
  group_by(Year) %>%
  summarise(Midpoint = mean(Day), .groups = "drop")

# Plot - NDVI with Points, Month on top & Year inside the plot
ggplot(data_summary, aes(x = Day)) +
  geom_point(aes(y = Avg_GOCI_NDVI, color = "GOCI NDVI"), size = 1.5) +
  geom_point(aes(y = Avg_MODIS_NDVI, color = "MODIS NDVI"), size = 1.5, shape = 17) +
  scale_y_continuous(
    name = "BAR NDVI",
    limits = c(0, 0.8)
  ) +
  scale_x_date(
    breaks = data_summary$Day,
    labels = format(data_summary$Day, "%b"),
    expand = c(0, 0)
  ) +
  labs(title = "Variation of GOCI and MODIS NDVI", x = NULL) +
  theme_minimal() +
  scale_color_manual(values = c("GOCI NDVI" = "black", "MODIS NDVI" = "gray")) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Add year labels INSIDE the plot at the midpoints
  geom_text(data = year_midpoints, aes(x = Midpoint, y = 0.7, label = Year),
            size = 5, fontface = "bold", color = "darkred")

