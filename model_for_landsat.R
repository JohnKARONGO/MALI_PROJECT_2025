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
library(camcorder)
library(janitor)
library(ggExtra)


# What This Code Does:
# 1.Slits data into training (2019-2021) and testing (2022).
# 2. reprocesses data (normalization, encoding).
# 3. rains Random Forest, XGBoost, SVM, KNN using tidymodels.
# 4. mplements an LSTM model in keras.
# 5. Tunes hyperparameters using cross-validation.
# 6. Computes performance metrics (RMSE, R², MAE).
# 7. redicts on test data and evaluates models.

gg_record(dir = "yield_march-figures/landsat/modelling_plots", device = "png", width = 9, height = 8, units = "in", dpi = 320)

# Set seed for reproducibility
set.seed(123)

# Load data (Assume df is already loaded)
# Filtering relevant columns
freq_df <- read_csv("mali_revised_April_2025.csv")%>% 
  clean_names()

df_landsat <- freq_df %>%
  select(year, cultivated_land_ha, yield_kg_per_ha, evi_landsat, ndvi_landsat, tmin, precip, tmax)

# check significance
lm_model_landsat <- lm(yield_kg_per_ha ~ evi_landsat + ndvi_landsat + tmin + precip + tmax, data = df_landsat)

summary(lm_model_landsat)

# Data Splitting
train_data_landsat <- df_landsat %>% filter(year >= 2019 & year <= 2021)
test_data_landsat <- df_landsat %>% filter(year == 2022)

# Drop year column for modeling
train_data_landsat <- train_data_landsat %>% select(-year)
test_data_landsat <- test_data_landsat %>% select(-year)

# Create a 10-fold cross-validation for model tuning
cv_folds_landsat <- vfold_cv(train_data_landsat, v = 10)

# Define a recipe for preprocessing
rec_landsat <- recipe(yield_kg_per_ha ~ ., data = train_data_landsat) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Define models
rf_model_landsat <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_model_landsat <- boost_tree(mtry = tune(), trees = 500, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

svm_model_landsat <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

knn_model_landsat <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Define workflows
workflows_list_landsat <- list(
  rf = workflow() %>% add_model(rf_model_landsat) %>% add_recipe(rec_landsat),
  xgb = workflow() %>% add_model(xgb_model_landsat) %>% add_recipe(rec_landsat),
  svm = workflow() %>% add_model(svm_model_landsat) %>% add_recipe(rec_landsat),
  knn = workflow() %>% add_model(knn_model_landsat) %>% add_recipe(rec_landsat)
)



# Define workflows
models_landsat <- workflow_set(
  preproc = list(rec_landsat),
  models = list(rf = rf_model_landsat, xgb = xgb_model_landsat, svm = svm_model_landsat, knn = knn_model_landsat)
)

# Tune hyperparameters using cross-validation
ctrl <- control_grid(save_pred = TRUE)

tuned_results_landsat <- models_landsat %>%
  workflow_map("tune_grid", resamples = cv_folds_landsat, grid = 10, metrics = metric_set(rmse, rsq, mae),
               control = ctrl)

# Extract best hyperparameters
best_params_landsat <- map(tuned_results_landsat$result, select_best, metric = "rmse")

original_workflows_landsat <- map(tuned_results_landsat$wflow_id, ~ extract_workflow(models_landsat, id = .x))

# Step 4: Finalize workflows using original workflow objects and best parameters
final_workflows_landsat <- map2(
  original_workflows_landsat,
  best_params_landsat,
  finalize_workflow
)

# Step 5: Extract predictions and tag with model name
predictions_list_landsat <- map2(
  tuned_results_landsat$result,
  tuned_results_landsat$wflow_id,
  ~ collect_predictions(.x) %>%
    mutate(Model = .y)  # Add model name for plotting
)

# Step 6: Refit final workflows on CV folds to get best predictions
best_predictions_list <- map2(
  final_workflows_landsat,
  tuned_results_landsat$wflow_id,
  ~ fit_resamples(
    .x,
    resamples = cv_folds_landsat,
    metrics = metric_set(rmse, rsq, mae),
    control = ctrl
  ) %>%
    collect_predictions() %>%
    mutate(Model = .y)  # Add model label
)


# Step 7: Combine all predictions into one dataframe
best_predictions_df_landsat <- bind_rows(best_predictions_list) %>% 
  mutate(split = "training_data") %>% 
  select(predicted = .pred, actual = yield_kg_per_ha,
         model = Model, split) %>% 
  mutate(model = case_when(model == "recipe_knn" ~ "knn",
                           model == "recipe_rf" ~ "rf",
                           model == "recipe_svm" ~ "svm", 
                           model == "recipe_xgb" ~ "xgb"))

# Extract Cross-Validation Metrics for RMSE, R², and MAE
cv_metrics_landsat <- map_dfr(tuned_results_landsat$result, ~ collect_metrics(.x) %>% 
                        filter(.metric %in% c("rmse", "rsq", "mae")))


# Fit final models
final_fits_landsat <- map(final_workflows_landsat, fit, data = train_data_landsat)

names(final_fits_landsat) <- c("knn", "rf", "svm", "xgb") 

# Make predictions on the test data (Ensuring correct alignment)
predictions_list_landsat <- map(final_fits_landsat, ~ predict(.x, new_data = test_data_landsat) %>% bind_cols(test_data_landsat %>% select(yield_kg_per_ha))) %>%
  map2_df(names(final_fits_landsat), ~ mutate(.x, model = .y)) %>%
  rename(predicted = .pred,
         actual = yield_kg_per_ha) %>%  # Rename prediction column for consistency
  mutate(split = "testing_data")



# Compute model performance metrics
test_metrics_landsat <- predictions_list_landsat %>%
  group_by(model) %>%
  summarise(
    rmse = rmse_vec(actual, predicted),
    rsq = rsq_vec(actual, predicted),
    mae = mae_vec(actual, predicted)
  )

print(test_metrics_landsat)

## update once we have true values of training metrics for satelites
train_metrics_landsat <- tibble(
  model = c("knn", "rf", "svm", "xgb"),
  rmse = c(0.560, 0.574, 0.539, 0.643),
  rsq = c(0.864, 0.861, 0.874, 0.835),
  mae = c(408, 420, 397, 452)
)


bind_all_landsat <- best_predictions_df_landsat %>% 
  bind_rows(predictions_list_landsat)

# Step 3: Create annotation labels
annot_data_landsat <- bind_all_landsat %>%
  left_join(test_metrics_landsat %>% select(model, test_rsq = rsq), by = "model") %>%
  left_join(train_metrics_landsat %>% select(model, train_rsq = rsq), by = "model") %>%
  group_by(model) %>%
  summarize(
    test_label = paste0("Testing R² = ", round(first(test_rsq), 2)),
    train_label = paste0("Training R² = ", round(first(train_rsq), 2)),
    x = max(actual, na.rm = TRUE),
    y = min(predicted, na.rm = TRUE),
    .groups = "drop"
  )


# Step 4: Plot the predictions with R² labels
ggplot(bind_all_landsat, aes(x = actual, y = predicted)) +
  geom_jitter(aes(color = split)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free_x") +
  geom_text(
    data = annot_data_landsat,
    aes(x = x, y = y, label = test_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  geom_text(
    data = annot_data_landsat,
    aes(x = x, y = y + 1000, label = train_label),  # Adjust height if needed
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  theme_minimal()


# The histogram
# Step 1: Create updated annotation labels for test and train R² and RMSE
annot_data_landsat <- bind_all_landsat  %>%
  left_join(test_metrics_landsat  %>% select(model, test_rsq = rsq, test_rmse = rmse), by = "model") %>%
  left_join(train_metrics_landsat  %>% select(model, train_rsq = rsq, train_rmse = rmse), by = "model") %>%
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
      data = annot_data_landsat  %>% filter(model == model_name),
      aes(x = x, y = y, label = test_label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 2, color = "black"
    ) +
    
    # Customizing axis to show (0, 0) clearly
    scale_x_continuous(limits = c(0, max(data$actual, na.rm = TRUE) * 1.1)) +  # Extend x-axis to include 0
    scale_y_continuous(limits = c(0, max(data$predicted, na.rm = TRUE) * 1.1)) +  # Extend y-axis to include 0
    theme_classic()
  
  # Add marginal histograms
  ggMarginal(p, type = "histogram", fill = "green", bins = 20)
}
p1_landsat <- plot_pred_vs_obs_with_hist(bind_all_landsat, "rf")
p2_landsat <- plot_pred_vs_obs_with_hist(bind_all_landsat, "knn")
p3_landsat <- plot_pred_vs_obs_with_hist(bind_all_landsat, "svm")
p4_landsat <- plot_pred_vs_obs_with_hist(bind_all_landsat, "xgb")



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

# Assuming bind_all_landsat is your data
p_knn <- plot_pred_obs_with_hist(bind_all_landsat, "knn")
p_rf  <- plot_pred_obs_with_hist(bind_all_landsat, "rf")
p_svm <- plot_pred_obs_with_hist(bind_all_landsat, "svm")
p_xgb <- plot_pred_obs_with_hist(bind_all_landsat, "xgb")

# Arrange in 3 columns x 4 rows (patchwork fills by row)
final_plot <- (
  p_knn + p_rf + p_svm + p_xgb +
    plot_layout(ncol = 3)
)

# Print all in one frame
final_plot
