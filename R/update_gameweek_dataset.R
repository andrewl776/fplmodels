#' Update Gameweek Dataset
#'
#' Update the cached RDS file
#'
#' @importFrom dplyr `%>%`
#' @export
#'
#'

library(dplyr)

update_gameweek_dataset <- function(commit = FALSE) {

  # withr::with_dir("C:\\Users\\alittle\\Documents\\Internal\\Personal development\\Personal_R\\fpl", {

  # Commit current data if there are any changes
  if (interactive() & commit) {
    git2r::add(path = "data/players_by_gameweek.rds")
    stat <- git2r::status()
    if (length(stat$staged) != 0) {
      git2r::config(user.name = 'andrewl776', user.email = 'andrewlittlebristol@gmail.com')
      git2r::commit(message = "Automatic update data commit.", )
    }
  }

  # Get new data
  gw_data <- fplr::fpl_get_player_all() %>%
    dplyr::mutate(
      "gameweek" =  dplyr::pull(fplr::fpl_get_gameweek_current(), "id"),
      "correct_as_of_time" = Sys.time()
    ) %>%
    dplyr::rename("player_id" = id)

  # Get old data
  old_data <- readr::read_rds("data/players_by_gameweek.rds")

  # Define what season it is
  max_row <- gw_data %>%
    dplyr::filter(correct_as_of_time == max(correct_as_of_time)) %>%
    dplyr::slice(1)

  if (max_row$gameweek == 1) {

    season <- fplr::fpl_get_gameweek_current() %>%
      dplyr::pull('deadline_time') %>%
      lubridate::year()

    if (!stringr::str_detect(season, '^20')) stop('Broken season calculation.')

    season <- season %>% stringr::str_remove('^20')
    season_next <- ((season %>% as.integer()) + 1) %>% as.character()

    season <- paste0(season, '/', season_next)

  } else {
    gw_data$season <- old_data$season %>% max()
  }

  # Bind with old data
  gw_data <- gw_data %>%
    dplyr::bind_rows(
      old_data %>%
        dplyr::mutate("gameweek" = as.integer(gameweek),
                      "news_added" = as.character(news_added))
    )

  # Add in additional columns to new data with a bit of cleaning (next_gw_points)
  # We also make sure we don't double count gameweeks here
  gw_data <- gw_data %>%
    dplyr::mutate("gameweek" = as.integer(gameweek)) %>%
    dplyr::group_by(player_id, gameweek, season) %>%
    dplyr::filter(correct_as_of_time == max(correct_as_of_time)) %>%
    dplyr::ungroup() %>%
    dplyr::select(web_name, gameweek, total_points, everything()) %>%
    dplyr::arrange(player_id, gameweek, season)

  # Only give next_gw_points if the next gameweek is available
  # (Sometimes we have the odd missing gameweek for old data)
  gw_data <- gw_data %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::mutate('next_gw_av' = (
      dplyr::lead(gameweek, order_by = gameweek) == gameweek + 1
    ))

  gw_data <- gw_data %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::mutate(next_gw_points = dplyr::if_else(
      next_gw_av,
      dplyr::lead(event_points, order_by = gameweek),
      NA_real_
    )) %>%
    dplyr::select(web_name, gameweek, event_points, next_gw_points, dplyr::everything()) %>%
    dplyr::ungroup()

  # Add in team name and position -------------------------------------------

  # Get premier league table of teams
  teams <- rvest::read_html("https://www.premierleague.com/tables") %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes(".short") %>%
    rvest::html_text() %>%
    .[1:20] %>%
    dplyr::tibble("team" = .) %>%
    dplyr::mutate("position" = dplyr::row_number())

  # Join with fpl API data to get IDs and full team names
  fpl_teams <- fplr::fpl_get_teams() %>%
    dplyr::select(-position) %>%
    dplyr::left_join(teams, c("short_name" = "team")) %>%
    dplyr::select(name, team_position = position, id)

  gw_data <- gw_data %>%
    dplyr::left_join(fpl_teams, c("team" = "id", "team_position" = "team_position")) %>%
    dplyr::mutate(team_name = name) %>%
    dplyr::select(-name, -team)

  # Write data to locations -------------------------------------------------

  gw_data %>%
    readr::write_csv("data/players_by_gameweek_csv.csv")

  gw_data %>%
    saveRDS("data/players_by_gameweek.rds")

  return(gw_data)
}


