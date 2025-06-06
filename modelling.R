library(tidyverse)
library(janitor)
library(camcorder)
library(tidymodels)

gg_record(dir = "yield_march-figures/modeling_figures", device = "png", width = 9, height = 8, units = "in", dpi = 320)

freq_df <- read_csv("mali_revised_April_2025.csv") %>% 
  clean_names()

# What This Code Does:
# 1.Slits data into training (2019-2021) and testing (2022).
# 2. reprocesses data (normalization, encoding).
# 3. rains Random Forest, XGBoost, SVM, KNN using tidymodels.
# 4. mplements an LSTM model in keras.
# 5. unes hyperparameters using cross-validation.
# 6. omputes performance metrics (RMSE, R², MAE).
# 7. redicts on test data and evaluates models.

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

gg_record(dir = "yield_march-figures/modelling_plots", device = "png", width = 9, height = 8, units = "in", dpi = 320)

# Set seed for reproducibility
set.seed(123)

# Load data (Assume df is already loaded)
# Filtering relevant columns
df <- freq_df %>%
  select(year, cultivated_land_ha, yield_kg_per_ha, evi_landsat, evi_modis, evi_sentinel, ndvi_landsat, ndvi_modis, ndvi_sentinel, tmin, precip, tmax)

## dO NOT RUN THIS ---
# check significance
lm_model <- lm(yield_kg_per_ha ~ cultivated_land_ha + evi_modis + evi_sentinel + ndvi_landsat + ndvi_modis + ndvi_sentinel + tmin + precip + tmax, data = freq_df)

summary(lm_model)

# Data Splitting
train_data <- df %>% filter(year >= 2019 & year <= 2021)
test_data <- df %>% filter(year == 2022)

# Drop year column for modeling
train_data <- train_data %>% select(-year)
test_data <- test_data %>% select(-year)

# Create a 5-fold cross-validation for model tuning
cv_folds <- vfold_cv(train_data, v = 5)

# Define a recipe for preprocessing
rec_landsat <- recipe(yield_kg_per_ha ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Define models
rf_model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_model <- boost_tree(mtry = tune(), trees = 500, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

svm_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Define workflows
workflows_list <- list(
  rf = workflow() %>% add_model(rf_model) %>% add_recipe(rec),
  xgb = workflow() %>% add_model(xgb_model) %>% add_recipe(rec),
  svm = workflow() %>% add_model(svm_model) %>% add_recipe(rec),
  knn = workflow() %>% add_model(knn_model) %>% add_recipe(rec)
)

# Define workflows
models <- workflow_set(
  preproc = list(rec),
  models = list(rf = rf_model, xgb = xgb_model, svm = svm_model, knn = knn_model)
)

# Tune hyperparameters using cross-validation

ctrl <- control_grid(save_pred = TRUE)

tuned_results <- models %>%
  workflow_map("tune_grid", resamples = cv_folds, grid = 10, metrics = metric_set(rmse, rsq, mae),
               control = ctrl)

# Extract best hyperparameters
best_params <- map(tuned_results$result, select_best, metric = "rmse")


# Step 4: Finalize workflows using original workflows and best parameters
final_workflows <- map2(tuned_results$workflow, best_params, finalize_workflow)

# Step 5: Refit final workflows on CV folds to get best predictions
best_predictions_list <- map2(
  final_workflows,
  tuned_results$wflow_id,
  ~ fit_resamples(
    .x,
    resamples = cv_folds,
    metrics = metric_set(rmse, rsq, mae),
    control = ctrl
  ) %>%
    collect_predictions() %>%
    mutate(Model = .y)  # Add model label
)



# Step 6: Combine all predictions into one dataframe
best_predictions_df <- bind_rows(best_predictions_list) %>% 
  mutate(split = "training_data") %>% 
  select(predicted = .pred, actual = yield_kg_per_ha,
         model = Model, split) %>% 
  mutate(model = case_when(model == "recipe_knn" ~ "knn",
                           model == "recipe_rf" ~ "rf",
                           model == "recipe_svm" ~ "svm", 
                           model == "recipe_xgb" ~ "xgb"))

# Extract Cross-Validation Metrics for RMSE, R², and MAE
cv_metrics <- map_dfr(tuned_results$result, ~ collect_metrics(.x) %>% 
                        filter(.metric %in% c("rmse", "rsq", "mae")))





# Fit final models
final_fits <- map(final_workflows, fit, data = train_data)

# Make predictions on the test data (Ensuring correct alignment)
predictions <- map(final_fits, ~ predict(.x, new_data = test_data) %>% bind_cols(test_data %>% select(yield_kg_per_ha))) %>%
  map2_df(names(final_fits), ~ mutate(.x, model = .y)) %>%
  rename(predicted = .pred,
         actual = yield_kg_per_ha) %>%  # Rename prediction column for consistency
  mutate(split = "testing_data")



# Compute model performance metrics
test_metrics <- predictions %>%
  group_by(model) %>%
  summarise(
    rmse = rmse_vec(actual, predicted),
    rsq = rsq_vec(actual, predicted),
    mae = mae_vec(actual, predicted)
  )

print(test_metrics)

## update once we have true values of training metrics for satelites
train_metrics <- tibble(
  model = c("knn", "rf", "svm", "xgb"),
  rmse = c(895, 554, 536, 609),
  rsq = c(0.671, 0.880, 0.869, 0.874),
  mae = c(607, 404, 396, 426)
)


bind_all <- best_predictions_df %>% 
  bind_rows(predictions)

# Step 3: Create annotation labels
annot_data <- bind_all %>%
  left_join(test_metrics %>% select(model, test_rsq = rsq), by = "model") %>%
  left_join(train_metrics %>% select(model, train_rsq = rsq), by = "model") %>%
  group_by(model) %>%
  summarize(
    test_label = paste0("Testing R² = ", round(first(test_rsq), 2)),
    train_label = paste0("Training R² = ", round(first(train_rsq), 2)),
    x = max(actual, na.rm = TRUE),
    y = min(predicted, na.rm = TRUE),
    .groups = "drop"
  )


# Step 4: Plot the predictions with R² labels
ggplot(bind_all, aes(x = actual, y = predicted)) +
  geom_jitter(aes(color = split)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free_x") +
  geom_text(
    data = annot_data,
    aes(x = x, y = y, label = test_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  geom_text(
    data = annot_data,
    aes(x = x, y = y + 1000, label = train_label),  # Adjust height if needed
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  theme_minimal()





# -------------------------------
# LSTM Model
# -------------------------------

library(dplyr)
library(keras)
library(tibble)
library(yardstick)
library(tensorflow)

# Convert categorical variables to dummy variables (one-hot encoding)
train_x <- train_data %>%
  select(-yield_kg) %>%
  mutate(satellite = as.factor(satellite)) %>%  # Ensure it's a factor
  model.matrix(~ . - 1, data = .) %>%  # One-hot encode factor columns
  as.matrix()

test_x <- test_data %>%
  select(-yield_kg) %>%
  mutate(satellite = as.factor(satellite)) %>%  # Ensure it's a factor
  model.matrix(~ . - 1, data = .) %>%  # One-hot encode factor columns
  as.matrix()

# Extract target variable
train_y <- train_data$yield_kg %>% as.matrix()
test_y <- test_data$yield_kg %>% as.matrix()

# Normalize data (only numeric values remain)
train_x <- scale(train_x)
test_x <- scale(test_x)

# Fix the reshaping issue: Use correct input shape
train_x <- array_reshape(train_x, c(nrow(train_x), 1, dim(train_x)[2]))  
test_x <- array_reshape(test_x, c(nrow(test_x), 1, dim(test_x)[2]))

# Define LSTM model
lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(1, dim(train_x)[3])) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 1)

# Compile LSTM model
lstm_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae", "mse")
)

# Train LSTM model
lstm_model %>% fit(train_x, train_y, epochs = 50, batch_size = 32, validation_split = 0.2)

# Predict using LSTM
lstm_pred <- lstm_model %>% predict(test_x)

# Compute LSTM performance metrics
lstm_metrics <- tibble(
  model = "LSTM",
  rmse = rmse_vec(truth = test_y, estimate = lstm_pred),
  rsq = rsq_vec(truth = test_y, estimate = lstm_pred),
  mae = mae_vec(truth = test_y, estimate = lstm_pred)
)

# Combine all results
final_results <- bind_rows(metrics, lstm_metrics)
print(final_results)
