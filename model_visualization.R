library(ggplot2)
library(dplyr)
library(camcorder)

gg_record(dir = "yield_march-figures/modeling_figures", device = "png", width = 9, height = 8, units = "in", dpi = 320)


# Create the full data frame including kNN
df <- data.frame(
  Model = rep(c("RF", "SVM", "XGB", "kNN"), each = 3),
  Satellite = rep(c("Landsat", "MODIS", "Sentinel"), times = 4),
  RMSE = c(0.550, 0.559, 0.556, 0.555, 0.578, 0.545, 0.564, 0.575, 0.573, 1.037, 1.235, 1.155),
  RSQ = c(0.880, 0.876, 0.877, 0.869, 0.866, 0.873, 0.874, 0.868, 0.871, 0.671, 0.572, 0.606)
)

# Create combined label
df$Model_Satellite <- paste0(tolower(df$Model), "_", tolower(df$Satellite))

# Reorder x-axis by Satellite first, then Model
satellite_order <- c("modis", "landsat", "sentinel")
model_order <- c("rf", "svm", "xgb", "knn")
desired_order <- as.vector(outer(model_order, satellite_order, paste, sep = "_"))
df$Model_Satellite <- factor(df$Model_Satellite, levels = desired_order)

# Plot
ggplot(df, aes(x = Model_Satellite)) +
  # RMSE bar plot
  geom_bar(aes(y = RMSE, fill = Satellite), stat = "identity", width = 0.7) +
  
  # RSQ line and points with new legend label
  geom_line(aes(y = RSQ, color = "R² Score", group = 1), size = 1.2) +
  geom_point(aes(y = RSQ, color = "R² Score"), size = 2) +
  
  # Add R² values on the points
  geom_text(aes(y = RSQ, label = round(RSQ, 2), color = "R² Score"), vjust = -0.5, size = 3) +
  
  # Define custom colors
  scale_fill_manual(values = c("Landsat" = "blue", "MODIS" = "red", "Sentinel" = "green")) +
  scale_color_manual(values = c("R² Score" = "black")) +
  
  # Axis settings
  scale_y_continuous(
    name = "RMSE (kg/ha)",
    limits = c(0, max(df$RMSE) + 0.3)
  ) +
  
  # Labels and theme
  labs(
    title = "Model Performance: RMSE vs R² across Satellite Sources",
    x = "Model and Satellite",
    fill = "Satellite",
    color = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black")
  )


library(ggplot2)
library(ggExtra)

set.seed(123)
results_df <- data.frame(
  actual = rnorm(100, mean = 50, sd = 10),
  predicted = rnorm(100, mean = 52, sd = 10)
)



