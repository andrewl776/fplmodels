#' @export

#gw_data <- update_gameweek_dataset()

apply_initial_prep_for_modelling <- function(gw_data) {

gw_data %>% delete_columns_with_many_NAs()

# Remove rows with NAs in target column
gw_data <- gw_data %>%
  drop_na(next_gw_points)

ncol(gw_data)

# Add in webname
gw_data <- gw_data %>%
  select(where(is.numeric)) %>%
  bind_cols(select(gw_data, web_name))

}
