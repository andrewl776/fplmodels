gw_data <- update_gameweek_dataset()

gw_data <- gw_data %>% 
  delete_columns_with_many_NAs()
