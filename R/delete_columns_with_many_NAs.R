delete_columns_with_many_NAs <- function(gw_data, max_proportion_na = 0.1) {
  
  # Find cols with low NAs
  cols <- gw_data %>% 
    select(-next_gw_points) %>% 
    summarise(across(.fns = ~ sum(is.na(.)))) %>% 
    pivot_longer(everything(), "column", values_to = "missing") %>% 
    filter(missing > nrow(gw_data)*max_proportion_na) %>% 
    pull("column")
  
  # Messages
  message(paste0(length(cols), " column(s) removed: ", "\n"))
  purrr::map(cols, ~ message(.x))
  message(paste0("\n", ncol(gw_data) - length(cols), " remaining"))
         
  # Return
  gw_data %>% 
    select(-any_of({{cols}}))
}
