#' Update Gameweek Dataset
#'
#' Update the cached RDS file
#'
#' @importFrom dplyr `%>%`
#' @export
#'
#'



update_gameweek_dataset <- function() {

  # withr::with_dir("C:\\Users\\alittle\\Documents\\Internal\\Personal development\\Personal_R\\fpl", {

  # Add in git2r commmit before any of this is done.

  # Get new data
  gw_data <- fplr::fpl_get_player_all() %>%
    dplyr::mutate(
      "gameweek" =  dplyr::pull(fplr::fpl_get_gameweek_current(), "id"),
      "correct_as_of_time" = Sys.time()
    ) %>%
    dplyr::rename("player_id" = id)

  # Bind with old data
  gw_data <- gw_data %>%
    dplyr::bind_rows(
      readr::read_rds("data/players_by_gameweek.rds") %>%
        dplyr::mutate("gameweek" = as.integer(gameweek))
    )

  # Add in additional columns to new data with a bit of cleaning (next_gw_points)
  # We also make sure we don't double count gameweeks here
  gw_data <- gw_data %>%
    dplyr::mutate("gameweek" = as.integer(gameweek)) %>%
    dplyr::group_by(player_id, gameweek) %>%
    dplyr::filter(correct_as_of_time == max(correct_as_of_time)) %>%
    dplyr::ungroup() %>%
    dplyr::select(web_name, gameweek, total_points, everything()) %>%
    dplyr::arrange(web_name, gameweek) %>%
    dplyr::group_by(web_name) %>%
    dplyr::mutate(next_gw_points = dplyr::lead(event_points, order_by = gameweek)) %>%
    dplyr::select(web_name, gameweek, event_points, next_gw_points, dplyr::everything()) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(next_gw_points)

  gw_data %>%
    saveRDS("data/players_by_gameweek.rds")

  return(gw_data)
  # })
}


# See https://github.com/KKulma/carbon-intensity-app/tree/main/.github/workflows
# might help with scheduling actions on a GH repo
