# Load required libraries
library(tidymodels)
library(tidyverse)
library(keras)
library(rsample)
library(parsnip)
library(yardstick)
library(workflows)
library(workflowsets)
library(kknn)
library(ggExtra)
library(camcorder)

gg_record(dir = "yield_march-figures/modis/modelling_plots", device = "png", width = 9, height = 8, units = "in", dpi = 320)

# Set seed for reproducibility
set.seed(123)

# Load data (Assume df is already loaded)
freq_df <- read_csv("mali_revised_April_2025.csv") %>% 
  clean_names()

df_modis <- freq_df %>% 
  select(year, cultivated_land_ha, yield_kg_per_ha, evi_modis, ndvi_modis, tmin, precip, tmax)

# check significance
lm_model_modis <- lm(yield_kg_per_ha ~ evi_modis + ndvi_modis  + tmin + precip + tmax, data = df_modis)

summary(lm_model_modis)

# Data Splitting
train_data_modis <- df_modis %>% filter(year >= 2019 & year <= 2021)
test_data_modis <- df_modis %>% filter(year == 2022)

# Drop year column for modeling
train_data_modis <- train_data_modis %>% select(-year)
test_data_modis <- test_data_modis %>% select(-year)

# Create a 10-fold cross-validation for model tuning
cv_folds_modis <- vfold_cv(train_data_modis, v = 10)

# Define a recipe for preprocessing
rec_modis <- recipe(yield_kg_per_ha ~ ., data = train_data_modis) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Define models
rf_model_modis <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_model_modis <- boost_tree(mtry = tune(), trees = 500, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

svm_model_modis <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

knn_model_modis <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Define workflows
workflows_list_modis <- list(
  rf = workflow() %>% add_model(rf_model_modis) %>% add_recipe(rec_modis),
  xgb = workflow() %>% add_model(xgb_model_modis) %>% add_recipe(rec_modis),
  svm = workflow() %>% add_model(svm_model_modis) %>% add_recipe(rec_modis),
  knn = workflow() %>% add_model(knn_model_modis) %>% add_recipe(rec_modis)
)

# Define workflows
models_modis <- workflow_set(
  preproc = list(rec_modis),
  models = list(rf = rf_model_modis, xgb = xgb_model_modis, svm = svm_model_modis, knn = knn_model_modis)
)

# Tune hyperparameters using cross-validation
ctrl <- control_grid(save_pred = TRUE)

tuned_results_modis <- models_modis %>%
  workflow_map("tune_grid", resamples = cv_folds_modis, grid = 10, metrics = metric_set(rmse, rsq, mae),
               control = ctrl)

# Extract best hyperparameters
best_params_modis <- map(tuned_results_modis$result, select_best, metric = "rmse")

original_workflows_modis <- map(tuned_results_modis$wflow_id, ~ extract_workflow(models_modis, id = .x))

# Step 4: Finalize workflows using original workflow objects and best parameters
final_workflows_modis <- map2(
  original_workflows_modis,
  best_params_modis,
  finalize_workflow
)

# Step 5: Extract predictions and tag with model name
predictions_list_modis <- map2(
  tuned_results_modis$result,
  tuned_results_modis$wflow_id,
  ~ collect_predictions(.x) %>%
    mutate(Model = .y)  # Add model name for plotting
)

# Step 6: Refit final workflows on CV folds to get best predictions
best_predictions_list_modis <- map2(
  final_workflows_modis,
  tuned_results_modis$wflow_id,
  ~ fit_resamples(
    .x,
    resamples = cv_folds_modis,
    metrics = metric_set(rmse, rsq, mae),
    control = ctrl
  ) %>%
    collect_predictions() %>%
    mutate(Model = .y)  # Add model label
)


# Step 7: Combine all predictions into one dataframe
best_predictions_df_modis <- bind_rows(best_predictions_list_modis) %>% 
  mutate(split = "training_data") %>% 
  select(predicted = .pred, actual = yield_kg_per_ha,
         model = Model, split) %>% 
  mutate(model = case_when(model == "recipe_knn" ~ "knn",
                           model == "recipe_rf" ~ "rf",
                           model == "recipe_svm" ~ "svm", 
                           model == "recipe_xgb" ~ "xgb"))

# Extract Cross-Validation Metrics for RMSE, R², and MAE
cv_metrics_modis <- map_dfr(tuned_results_modis$result, ~ collect_metrics(.x) %>% 
                              filter(.metric %in% c("rmse", "rsq", "mae")))


# Fit final models
final_fits_modis <- map(final_workflows_modis, fit, data = train_data_modis)

names(final_fits_modis) <- c("knn", "rf", "svm", "xgb") 

# Make predictions on the test data (Ensuring correct alignment)
predictions_list_modis <- map(final_fits_modis, ~ predict(.x, new_data = test_data_modis) %>% bind_cols(test_data_modis %>% select(yield_kg_per_ha))) %>%
  map2_df(names(final_fits_modis), ~ mutate(.x, model = .y)) %>%
  rename(predicted = .pred,
         actual = yield_kg_per_ha) %>%  # Rename prediction column for consistency
  mutate(split = "testing_data")



# Compute model performance metrics
test_metrics_modis <- predictions_list_modis %>%
  group_by(model) %>%
  summarise(
    rmse = rmse_vec(actual, predicted),
    rsq = rsq_vec(actual, predicted),
    mae = mae_vec(actual, predicted)
  )

print(test_metrics_modis)

## update once we have true values of training metrics for satelites
train_metrics_modis <- tibble(
  model = c("knn", "rf", "svm", "xgb"),
  rmse = c(0.560, 0.577, 0.539, 0.739),
  rsq = c(0.865, 0.856, 0.873, 0.789),
  mae = c(0.402, 0.411, 0.401, 0.500)
)


bind_all_modis <- best_predictions_df_modis %>% 
  bind_rows(predictions_list_modis)

# Step 3: Create annotation labels
annot_data_modis <- bind_all_modis %>%
  left_join(test_metrics_modis %>% select(model, test_rsq = rsq), by = "model") %>%
  left_join(train_metrics_modis %>% select(model, train_rsq = rsq), by = "model") %>%
  group_by(model) %>%
  summarize(
    test_label = paste0("Testing R² = ", round(first(test_rsq), 2)),
    train_label = paste0("Training R² = ", round(first(train_rsq), 2)),
    x = max(actual, na.rm = TRUE),
    y = min(predicted, na.rm = TRUE),
    .groups = "drop"
  )


# Step 4: Plot the predictions with R² labels
ggplot(bind_all_modis, aes(x = actual, y = predicted)) +
  geom_jitter(aes(color = split)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free_x") +
  geom_text(
    data = annot_data_modis,
    aes(x = x, y = y, label = test_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  geom_text(
    data = annot_data_modis,
    aes(x = x, y = y + 1000, label = train_label),  # Adjust height if needed
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  theme_minimal()

# Step 4: Plot the predictions with R² labels
ggplot(bind_all_modis, aes(x = actual, y = predicted)) +
  geom_jitter(aes(color = split)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free_x") +
  geom_text(
    data = annot_data_modis,
    aes(x = x, y = y, label = test_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  geom_text(
    data = annot_data_modis,
    aes(x = x, y = y + 1000, label = train_label),  # Adjust height if needed
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Enlarged facet labels
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Centered plot title
  ) +
  ggtitle("modis")


# Step 1: Create updated annotation labels for test and train R² and RMSE
annot_data_modis <- bind_all_modis %>%
  left_join(test_metrics_modis %>% select(model, test_rsq = rsq, test_rmse = rmse), by = "model") %>%
  left_join(train_metrics_modis %>% select(model, train_rsq = rsq, train_rmse = rmse), by = "model") %>%
  group_by(model) %>%
  summarize(
    test_label = paste0("R² = ", round(first(test_rsq), 2), 
                        "\nRMSE = ", round(first(test_rmse), 2)),
    train_label = paste0("Training R² = ", round(first(train_rsq), 2), 
                         "\nTraining RMSE = ", round(first(train_rmse), 2)),
    x = max(actual, na.rm = TRUE),
    y = min(predicted + 2500, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Define the function with theme_classic
plot_pred_vs_obs_with_hist <- function(data, model_name) {
  # Filter and build base scatter plot
  p <- data %>%
    filter(model == model_name, split == "testing_data") %>%
    ggplot(aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.7, color = "steelblue") +
    
    # Ensure the abline starts at (0, 0) with slope = 1
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    
    labs(
      x = "Observed",
      y = "Predicted"
    ) +
    
    # Adding annotation for R² and RMSE
    geom_text(
      data = annot_data_modis %>% filter(model == model_name),
      aes(x = x, y = y, label = test_label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 2, color = "black"
    ) +
    
    # Customizing axis to show (0, 0) clearly
    scale_x_continuous(limits = c(0, max(data$actual, na.rm = TRUE) * 1.1)) +  # Extend x-axis to include 0
    scale_y_continuous(limits = c(0, max(data$predicted, na.rm = TRUE) * 1.1)) +  # Extend y-axis to include 0
    theme_classic()
  
  # Add marginal histograms
  ggMarginal(p, type = "histogram", fill = "red", bins = 20)
}

# Step 3: Generate the plot for a model (e.g., rf)
p1_modis <- plot_pred_vs_obs_with_hist(bind_all_modis, "rf")
p2_modis <- plot_pred_vs_obs_with_hist(bind_all_modis, "knn")
p3_modis <- plot_pred_vs_obs_with_hist(bind_all_modis, "svm")
p4_modis <- plot_pred_vs_obs_with_hist(bind_all_modis, "xgb")

## Alternative code for plots

library(patchwork)

# Custom plot function using ggplot2 only (with manual histogram)
plot_pred_obs_with_hist <- function(data, model_name) {
  filtered_data <- data %>%
    filter(model == model_name, split == "testing_data")
  
  # Main scatter plot
  scatter <- ggplot(filtered_data, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.7, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Predicted vs Observed -", toupper(model_name)),
         x = "Observed", y = "Predicted") +
    theme_minimal(base_size = 10)
  
  # Histogram for predicted values
  hist_top <- ggplot(filtered_data, aes(x = predicted)) +
    geom_histogram(fill = "grey", bins = 20, color = "black") +
    theme_void()
  
  # Histogram for actual values
  hist_right <- ggplot(filtered_data, aes(x = actual)) +
    geom_histogram(fill = "grey", bins = 20, color = "black") +
    coord_flip() +
    theme_void()
  
  # Combine the three using patchwork layout
  (hist_top + plot_spacer()) /
    (scatter + hist_right) +
    plot_layout(widths = c(4, 1), heights = c(1, 4))
}

# Assuming bind_all_modis is your data
p_knn <- plot_pred_obs_with_hist(bind_all_modis, "knn")
p_rf  <- plot_pred_obs_with_hist(bind_all_modis, "rf")
p_svm <- plot_pred_obs_with_hist(bind_all_modis, "svm")
p_xgb <- plot_pred_obs_with_hist(bind_all_modis, "xgb")

# Arrange in 3 columns x 4 rows (patchwork fills by row)
final_plot <- (
  p_knn + p_rf + p_svm + p_xgb +
    plot_layout(ncol = 3)
)

# Print all in one frame
final_plot

