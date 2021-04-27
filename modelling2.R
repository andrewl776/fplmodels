
# Modelling ---------------------------------------------------------------

set.seed(101)
library(tidymodels)
library(tidyverse)


# Split data and create CV folds
gw_split <- initial_split(gw_data, 0.8, "next_gw_points")
gw_test <- testing(gw_split)
gw_train <- training(gw_split)
gw_folds <- vfold_cv(gw_train, 8, strata = "next_gw_points")

# Preprocessing
rec_dummy <- recipe(next_gw_points ~ ., data = gw_train) %>%
  step_rm(web_name) %>% 
  step_dummy(is.factor) #%>% 
step_log( 
  next_gw_points, 
  offset = 1-min(gw_data$next_gw_points)
) # We can have negtive pts so need offset

# Define model and workflow
model_spec_xg_tune <- boost_tree(
  trees = tune()
) %>% 
  set_engine("xgboost", importance = "impurity") %>% 
  set_mode("regression")



# #install.packages("ranger")
# rf_xy_fit <- 
#   rf_defaults %>%
#   set_engine("ranger", importance = "impurity") %>%
#   fit_xy(
#     x = ames_train %>% select(-Sale_Price),
#     y = log10(ames_train$Sale_Price)
#   )

best_vars <- rf_xy_fit %>% 
  #vip::vip(num_features = 70)
  vip::vi()


# Define single and tune workflows
xg_wf_tune <- workflow() %>% 
  add_recipe(rec_dummy) %>% 
  add_model(model_spec_xg_tune)

xg_wf_single <- workflow() %>% 
  add_recipe(rec_dummy) %>% 
  add_model(model_spec_xg_single)

xg_fit_vip <- model_spec_xg_single %>% 
  fit_xy(
    gw_train %>% select(-next_gw_points),
    gw_train$next_gw_points
  )

xg_fit_vip %>% vip::vip(num_features = 60)

# We need to more EDA  

# Define grid for tuning
xg_grid <- grid_random(trees(), size = 10)

# Fit single model
xg_fit_single <- fit_resamples(
  xg_wf_single,
  gw_folds, 
  control = control_resamples(save_pred = TRUE)
)

# Inverse log the predictions & response
if("step_log" %in% {rec_dummy$steps %>% 
    lapply(class) %>% unlist}) {
  
  xg_fit_single$.predictions <- xg_fit_single$.predictions %>%
    map(function(df) {
      df %>% mutate(across(
        c(.pred, next_gw_points),
        ~ exp(.)
      ))
    }
    # Collect predictions
    xg_preds_single <- xg_fit_single %>% 
      collect_predictions() %>% 
      mutate(across(c(.pred, next_gw_points), ~ exp(.)))
    )}

# Collect metrics from single model
# We have update predictions so need to call `metrics` on them
xg_fit_single %>% collect_predictions() %>% 
  metrics(.pred, next_gw_points)

# Results with ~ 700 rows
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard       2.49 
# 2 rsq     standard       0.239
# 3 mae     standard       1.44 

# Results with ~ 1400 rows


xg_fit_single %>% last_fit(gw_split) 

xg_fit_no_resamp_single <- xg_wf_single %>% 
  fit(gw_train)


# No exp required
xg_results <- xg_fit_no_resamp_single %>% 
  predict(gw_test) %>% 
  bind_cols(select(gw_test, next_gw_points))

xg_fit_no_resamp_single %>% 
  vip::vi()

# Create plotting function and execute
xy_plot <- function(preds, pred_col, target_col) {
  preds %>% ggplot(
    aes(x = {{target_col}}, y = {{pred_col}})
  ) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0)
}

xy_plot(xg_preds_single, .pred, next_gw_points)

xg_fit_tune <- tune_grid(
  xg_wf_tune,
  resamples = gw_folds, 
  grid = xg_grid
)

# Find best hyperparameters
xg_fit_tune %>% 
  show_best("rmse")

best_xg_params <- xg_fit_tune %>% 
  select_best("rmse")
best_xg_params


# We actually get a lower value here for our tuned model, than we do for the default, this suggest
# that our grid is not very optimal. This is also confirmed by the fact that our optimal parameters sit 
# right on the edge of our grid values, i.e. we are optimal at the highest `trees` value and 
# at the lowest `min_n` value.

# Finalise workflow and score
final_xg_wf <- xg_wf_tune %>% 
  finalize_workflow(best_xg_params)


final_xg_fit <- final_xg_wf %>% 
  last_fit(gw_split)

final_xg_fit %>% collect_metrics()

final_xg_fit %>% collect_predictions() %>% 
  bind_cols(select(gw_test, -next_gw_points)) %>% 
  select(.pred, next_gw_points, everything()) %>% 
  arrange(-next_gw_points)



# Functionising modelling -------------------------------------------------

fit_xg_single <- function(data, predictors = "all", seed = 101) {
  
  # Load in packages
  set.seed(seed)
  library(tidymodels)
  library(tidyverse)
  
  # Split data and create CV folds
  gw_split <- initial_split(gw_data, 0.8, "next_gw_points")
  gw_test <- testing(gw_split)
  gw_train <- training(gw_split)
  gw_folds <- vfold_cv(gw_train, 8, strata = "next_gw_points")
  
  # Define single model spec
  model_spec_xg_single <- boost_tree() %>% 
    set_engine("xgboost", importance = "impurity") %>% 
    set_mode("regression")
  
  
}
