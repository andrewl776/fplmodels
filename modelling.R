

# Load in / create data ---------------------------------------------------

players_w_teams_historic <- readr::read_rds("data/players_w_teams_historic")

players_next_season_pts <- players_w_teams_historic %>%
  select(web_name, season_name, total_points, everything()) %>% 
  arrange(web_name, season_name) %>% 
  group_by(web_name) %>% 
  mutate(next_season_points = lead(total_points, order_by = season_name)) %>% 
  select(web_name, season_name, total_points, next_season_points, everything()) %>% 
  ungroup()
players_next_season_pts

players_next_season_pts %>% 
  is.na() %>% 
  colSums()

# -------------------------------------------------------------------------
# MODELLING
# -------------------------------------------------------------------------

library(tidymodels)
library(tidyverse)

# Initial processing
historic_data <- players_next_season_pts %>% 
  drop_na() %>% 
  mutate(season_name = str_sub(season_name, 1, 4)) %>% 
  mutate(season_name = as.numeric(season_name)) %>% 
  mutate(across(-web_name, as.numeric))

# EDA
historic_data %>% glimpse()
historic_data %>%
  ggplot(aes(x = next_season_points)) +
  geom_density()

# Train test split
historic_split <- initial_split(historic_data, 0.75, strata = "next_season_points")
train_historic <- training(historic_split)
test_historic <- testing(historic_split)

rec_historic <- recipe(train_historic, next_season_points ~ .) %>% 
  step_log(next_season_points, offset = 1) %>% 
  step_center(season_name) %>% 
  step_rm(web_name, id) %>% 
  prep(train_historic)

## Converting to WF
#test_baked_historic <- bake(rec_historic, test_historic)
#train_baked_historic <- bake(rec_historic, train_historic)

model_historic <- decision_tree("regression") %>% 
  set_engine("rpart", model = TRUE) 

 workflow_historic <- workflow() %>% 
   add_recipe(rec_historic) %>% 
   add_model(model_historic)

model_fit_historic <- workflow_historic %>% 
  fit(data = train_historic)   # Note how we do not bake here as our WF will do it for us

#rpart.plot::rpart.plot(model_fit_historic$fit)

predictions <- bind_cols(
  test_baked_historic %>% select(next_season_points),
  predict(model_fit_historic, test_historic),
  select(test_historic, -next_season_points)
) %>% 
  mutate(across(c(.pred, next_season_points), .fns = exp)) %>% 
  mutate(next_season_points = round(next_season_points))

# round here as they are only non-integer because of log, exp 
# rounding area

# How did we do? Looks okay ish to me
# Need to use CV here really as the values vary quite wildly for different train/test splits.
metrics(
  predictions, 
  truth = next_season_points, 
  estimate = .pred
)

# What team would we have got?
predicted_11 <- predictions %>% 
  filter(season_name == 2018) %>% # whwere we
  arrange(-.pred) %>% 
  select(web_name, everything()) %>% 
  slice(1:11)
predicted_11  

predicted_11 %>% summarise(sum(next_season_points))


top_last_season <- test_historic %>% 
  filter(season_name == 2018) %>%
  arrange(-total_points) %>% 
  slice(1:11)

top_this_season <- test_historic %>% 
  filter(season_name == 2018) %>%
  arrange(-next_season_points) %>% 
  slice(1:11)


predicted_11 %>% summarise(mean(next_season_points))
test_historic %>% summarise(mean(next_season_points))
top_last_season %>% summarise(mean(next_season_points))
top_this_season %>% summarise(mean(next_season_points))


# Same mean number of points but predicted 5 of top 11
# whereas by_total_points technique only gets 4 of top 11

# Not a very good model to be honest, need to reduce # vars

predicted_11 %>% 
  inner_join(top_this_season, "web_name") %>% 
  arrange(-next_season_points.x)

top_last_season %>% 
  inner_join(top_this_season, "web_name") %>% 
  arrange(-next_season_points.x)



# TUNING --------------------------------------------------------------------------------------

all_cores <- parallel::detectCores(logical = FALSE)
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)


s <- Sys.time()
set.seed(2020)
xgb_res <- tune_grid(
  xgb_wf, #workflow
  resamples = cv_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE,
                         verbose = TRUE)
)
Sys.time() - s

xgb_res 