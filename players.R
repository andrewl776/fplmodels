library(fplr)
library(tidyverse)
library(tidymodels)
library(parallel)
library(taskscheduleR)

players <- fplr::fpl_get_player_all()

# head(players)
# names(players)
# 
# players_w_teams <- players %>% 
#   dplyr::left_join(
#     dplyr::select(teams, c(code, name)), 
#     by = c("team_code" = "code")
#   ) %>% 
#   dplyr::rename(team_name = name) %>% 
#   dplyr::arrange(team_name) %>% 
#   dplyr::select(first_name, second_name, team_name, everything()) %>% 
#   dplyr::left_join(
#     dplyr::select()
#   )


# # Sequential --------------------------------------------------------------
# 
# players_w_teams_historic <- purrr::map(
#   players$id, function(s) {
#     message(s)
#     tryCatch({
#       fplr::fpl_get_player_historic(s) %>% 
#         dplyr::mutate(id = s)
#     }, error = function(e) {
#       message(e, ": ", s)
#     })
#   }
# ) %>% 
#   dplyr::bind_rows() %>% 
#   dplyr::full_join(
#     players_w_teams,
#   )


# Parallel ----------------------------------------------------------------

# num_cores <- parallel::detectCores()
# cl <- parallel::makeCluster(num_cores)
# 
# parallel::clusterExport(cl, "players", envir = environment())
# 
# parallel::clusterEvalQ(cl, {
#   invisible(library(tidyverse))
# })
# 
# print(system.time(
#   
#   players_w_teams_historic <- parallel::parLapply(
#     cl, 
#     players$id, 
#     function(s) {
#       
#       message(s)
#       
#       tryCatch({
#         fplr::fpl_get_player_historic(s) %>% 
#           dplyr::mutate(id = s)
#       }, error = function(e) {
#         message(e, ": ", s)
#       })
#       
#     }) %>% 
#     dplyr::bind_rows()
# ))
# 
# parallel::stopCluster(cl)
# 
# 
# players_w_teams_historic %>% head
# 
# players_w_teams_historic %>% 
#   lapply(length) %>% unlist() %>% mean
# 
# 
# 
# players_w_teams_historic$total %>% 
#   dplyr::group_by(team_name) %>% 
#   filter(total_points)




# Currently we seem to only be able to do this at a year level (i.e. not gameweek)
# We will be able to do it by gameweek if each time we save and RDS object, the
# output of the function 


# Update dataset ----------------------------------------------------------


## Schedule to run every day
#taskscheduler_create(
#  taskname = "update_players_by_gameweek", 
#  rscript = normalizePath("update_gameweek_dataset.R"), 
#  schedule = "MINUTE", 
#  starttime = format(Sys.time() + 5, "%H:%M")
#)
#
#taskscheduler_ls()
#taskscheduler_delete("update_players_by_gameweek")


update_gameweek_dataset()



# Players by year ---------------------------------------------------------

#players_w_teams_historic %>% 
#  left_join(select(players, web_name, id), by = "id") %>% 
#  saveRDS("data/players_w_teams_historic")



# Around 25% of player years do not have a "next season". This isn't
# too bad and so for now we just remove them as we are building an MVP


# tidymodels --------------------------------------------------------------
