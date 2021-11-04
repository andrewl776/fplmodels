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

  # Commit current data if there are any changes
  if (interactive()) {
    stat <- git2r::status()
    if (length(stat$staged) != 0) {
      git2r::add(path = "data/players_by_gameweek.rds")
      git2r::commit(message = "Automatic update data commit.")
    }
  }

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
    dplyr::arrange(player_id, gameweek) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(next_gw_points = dplyr::lead(event_points, order_by = gameweek)) %>%
    dplyr::select(web_name, gameweek, event_points, next_gw_points, dplyr::everything()) %>%
    dplyr::ungroup()

  gw_data %>%
    saveRDS("data/players_by_gameweek.rds")

  return(gw_data)
}


