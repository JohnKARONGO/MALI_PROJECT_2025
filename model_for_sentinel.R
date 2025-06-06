# Load required libraries
library(tidymodels)
library(tidyverse)
library(keras)
library(rsample)
library(parsnip)
library(yardstick)
library(workflows)
library(workflowsets)
library(ggExtra)
library(kknn)

gg_record(dir = "yield_march-figures/modeling_figures/sentinel/", device = "png", width = 9, height = 8, units = "in", dpi = 320)

# Set seed for reproducibility
set.seed(123)

# Load data (Assume df is already loaded)
# Filtering relevant columns
freq_df <- read_csv("mali_revised_April_2025.csv") %>%  
  clean_names()

df_sentinel <- freq_df %>% 
  select(year, cultivated_land_ha, yield_kg_per_ha, evi_sentinel, ndvi_sentinel, tmin, precip, tmax)

# check significance
lm_model_sentinel <- lm(yield_kg_per_ha ~ evi_sentinel + ndvi_sentinel + tmin + precip + tmax, data = df_sentinel)

summary(lm_model_sentinel)

# Data Splitting
train_data_sentinel <- df_sentinel %>% filter(year >= 2019 & year <= 2021)
test_data_sentinel <- df_sentinel %>% filter(year == 2022)

# Drop year column for modeling
train_data_sentinel <- train_data_sentinel %>% select(-year)
test_data_sentinel <- test_data_sentinel %>% select(-year)

# Create a 10-fold cross-validation for model tuning
cv_folds_sentinel <- vfold_cv(train_data_sentinel, v = 10)

# Define a recipe for preprocessing
rec_sentinel <- recipe(yield_kg_per_ha ~ ., data = train_data_sentinel) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Define models
rf_model_sentinel <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_model_sentinel <- boost_tree(mtry = tune(), trees = 500, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

svm_model_sentinel <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

knn_model_sentinel <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Define workflows
workflows_list_sentinel <- list(
  rf = workflow() %>% add_model(rf_model_sentinel) %>% add_recipe(rec_sentinel),
  xgb = workflow() %>% add_model(xgb_model_sentinel) %>% add_recipe(rec_sentinel),
  svm = workflow() %>% add_model(svm_model_sentinel) %>% add_recipe(rec_sentinel),
  knn = workflow() %>% add_model(knn_model_sentinel) %>% add_recipe(rec_sentinel)
)

# Define workflows
models_sentinel <- workflow_set(
  preproc = list(rec_sentinel),
  models = list(rf = rf_model_sentinel, xgb = xgb_model_sentinel, svm = svm_model_sentinel, knn = knn_model_sentinel)
)


# Tune hyperparameters using cross-validation
ctrl <- control_grid(save_pred = TRUE)

# Tune hyperparameters using cross-validation
tuned_results_sentinel <- models_sentinel %>%
  workflow_map("tune_grid", resamples = cv_folds_sentinel, grid = 10, metrics = metric_set(rmse, rsq, mae))


# Extract best hyperparameters
best_params_sentinel <- map(tuned_results_sentinel$result, select_best, metric = "rmse")

original_workflows_sentinel <- map(tuned_results_sentinel$wflow_id, ~ extract_workflow(models_sentinel, id = .x))

# Step 4: Finalize workflows using original workflow objects and best parameters
final_workflows_sentinel <- map2(
  original_workflows_sentinel,
  best_params_sentinel,
  finalize_workflow
)

# Step 5: Extract predictions and tag with model name
predictions_list_sentinel <- map2(
  tuned_results_sentinel$result,
  tuned_results_sentinel$wflow_id,
  ~ collect_predictions(.x) %>%
    mutate(Model = .y)  # Add model name for plotting
)

# Step 6: Refit final workflows on CV folds to get best predictions
best_predictions_list_sentinel <- map2(
  final_workflows_sentinel,
  tuned_results_sentinel$wflow_id,
  ~ fit_resamples(
    .x,
    resamples = cv_folds_sentinel,
    metrics = metric_set(rmse, rsq, mae),
    control = ctrl
  ) %>%
    collect_predictions() %>%
    mutate(Model = .y)  # Add model label
)


# Step 7: Combine all predictions into one dataframe
best_predictions_df_sentinel <- bind_rows(best_predictions_list) %>% 
  mutate(split = "training_data") %>% 
  select(predicted = .pred, actual = yield_kg_per_ha,
         model = Model, split) %>% 
  mutate(model = case_when(model == "recipe_knn" ~ "knn",
                           model == "recipe_rf" ~ "rf",
                           model == "recipe_svm" ~ "svm", 
                           model == "recipe_xgb" ~ "xgb"))

# Extract Cross-Validation Metrics for RMSE, R², and MAE
cv_metrics_sentinel <- map_dfr(tuned_results_sentinel$result, ~ collect_metrics(.x) %>% 
                                 filter(.metric %in% c("rmse", "rsq", "mae")))


# Fit final models
final_fits_sentinel <- map(final_workflows_sentinel, fit, data = train_data_sentinel)

names(final_fits_sentinel) <- c("knn", "rf", "svm", "xgb") 

# Make predictions on the test data (Ensuring correct alignment)
predictions_list_sentinel <- map(final_fits_sentinel, ~ predict(.x, new_data = test_data_sentinel) %>% bind_cols(test_data_sentinel %>% select(yield_kg_per_ha))) %>%
  map2_df(names(final_fits_sentinel), ~ mutate(.x, model = .y)) %>%
  rename(predicted = .pred,
         actual = yield_kg_per_ha) %>%  # Rename prediction column for consistency
  mutate(split = "testing_data")



# Compute model performance metrics
test_metrics_sentinel <- predictions_list_sentinel %>%
  group_by(model) %>%
  summarise(
    rmse = rmse_vec(actual, predicted),
    rsq = rsq_vec(actual, predicted),
    mae = mae_vec(actual, predicted)
  )

print(test_metrics_sentinel)

## update once we have true values of training metrics for satelites
train_metrics_sentinel <- tibble(
  model = c("knn", "rf", "svm", "xgb"),
  rmse = c(0.560, 0.574, 0.539, 0.643),
  rsq = c(0.864, 0.861, 0.874, 0.835),
  mae = c(0.408, 0.420, 0.397, 0.452)
)


bind_all_sentinel <- best_predictions_df_sentinel %>% 
  bind_rows(predictions_list_sentinel)

# Step 3: Create annotation labels
annot_data_sentinel <- bind_all_sentinel %>%
  left_join(test_metrics_sentinel %>% select(model, test_rsq = rsq), by = "model") %>%
  left_join(train_metrics_sentinel %>% select(model, train_rsq = rsq), by = "model") %>%
  group_by(model) %>%
  summarize(
    test_label = paste0("Testing R² = ", round(first(test_rsq), 2)),
    train_label = paste0("Training R² = ", round(first(train_rsq), 2)),
    x = max(actual, na.rm = TRUE),
    y = min(predicted, na.rm = TRUE),
    .groups = "drop"
  )


# Step 4: Plot the predictions with R² labels
ggplot(bind_all_sentinel, aes(x = actual, y = predicted)) +
  geom_jitter(aes(color = split)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free_x") +
  geom_text(
    data = annot_data_sentinel,
    aes(x = x, y = y, label = test_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  geom_text(
    data = annot_data_sentinel,
    aes(x = x, y = y + 1000, label = train_label),  # Adjust height if needed
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3, color = "black"
  ) +
  theme_minimal()


# The histogram

# Create the base ggplot
p_knn <- ggplot(bind_all_sentinel %>% filter(model == 
                                               "knn"), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Observed", y = "Predicted", title = "Predicted vs Observed") +
  theme_minimal()

# Add marginal histograms
ggMarginal(p_knn, type = "histogram", fill = "grey", bins = 20)


p_rf <- ggplot(bind_all_sentinel %>% filter(model == "rf", 
                                            split == "testing_data"), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Observed", y = "Predicted", title = "Predicted vs Observed") +
  theme_minimal()

# Add marginal histograms
ggMarginal(p_rf, type = "histogram", fill = "grey", bins = 20)


# Step 1: Create updated annotation labels for test and train R² and RMSE
annot_data_sentinel <- bind_all_sentinel %>%
  left_join(test_metrics_sentinel %>% select(model, test_rsq = rsq, test_rmse = rmse), by = "model") %>%
  left_join(train_metrics_sentinel %>% select(model, train_rsq = rsq, train_rmse = rmse), by = "model") %>%
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
      data = annot_data_sentinel %>% filter(model == model_name),
      aes(x = x, y = y, label = test_label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 2, color = "black"
    ) +
    
    # Customizing axis to show (0, 0) clearly
    scale_x_continuous(limits = c(0, max(data$actual, na.rm = TRUE) * 1.1)) +  # Extend x-axis to include 0
    scale_y_continuous(limits = c(0, max(data$predicted, na.rm = TRUE) * 1.1)) +  # Extend y-axis to include 0
    theme_classic()
  
  # Add marginal histograms
  ggMarginal(p, type = "histogram", fill = "blue", bins = 20)
}

p1_sentinel <- plot_pred_vs_obs_with_hist(bind_all_sentinel, "rf")
p2_sentinel <- plot_pred_vs_obs_with_hist(bind_all_sentinel, "knn")
p3_sentinel <- plot_pred_vs_obs_with_hist(bind_all_sentinel, "svm")
p4_sentinel <- plot_pred_vs_obs_with_hist(bind_all_sentinel, "xgb")




library(ggpubr)

ggarrange(ncol = 4, nrow = 3,
          labels = c("RF", "kNN", "SVM", "XGB"),
  p1_landsat, p2_landsat, p3_landsat, p4_landsat,
  p1_modis, p2_modis, p3_modis, p4_modis,
  p1_sentinel, p2_sentinel, p3_sentinel, p4_sentinel)

# The ANOVA dataframe
anova_df <- bind_all_landsat %>% filter(model == "svm", split == "testing_data") %>% select(actual, predicted_landsat = predicted) %>% cbind(bind_all_modis %>% filter(model == "svm", split == "testing_data") %>% select( predicted_modis = predicted)) %>% cbind(bind_all_sentinel %>% filter(model == "svm", split == "testing_data") %>% select( predicted_sentinel = predicted))


# Load necessary packages
library(tidyverse)
library(broom)

# Reshape data for ANOVA
data_long <- anova_df %>%
  pivot_longer(cols = starts_with("predicted"), 
               names_to = "model", 
               values_to = "predicted_value") %>% 
  mutate(model = as.factor(model))

# Perform ANOVA to compare means of predicted values across different models
anova_result <- aov(predicted_value ~ model, data = data_long)

# View summary of ANOVA
summary(anova_result)

# Use broom to tidy the output into a clean table
anova_tidy <- tidy(anova_result)
print(anova_tidy)

# Perform pairwise t-tests between models
t_test_results <- pairwise.t.test(data_long$predicted_value, data_long$model, p.adjust.method = "bonferroni")

# Use broom to tidy the t-test results
t_test_tidy <- tidy(t_test_results)
print(t_test_tidy)


t.test(anova_df$predicted_landsat, anova_df$actual)
t.test(anova_df$predicted_modis, anova_df$actual)
t.test(anova_df$predicted_sentinel, anova_df$actual)
