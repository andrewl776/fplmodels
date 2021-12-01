#setwd("C:/Users/alittle/Documents/Internal/Personal_development/Personal_R/fplmodels")
tryCatch(
  fplmodels::update_gameweek_dataset(),
  error = function(e) {
    message(e)
    update_gameweek_dataset()
  }
)
# library(taskscheduleR)
# taskscheduler_create(
#   taskname = "update_fpl_data",
#   rscript = "C:/Users/alittle/Documents/Internal/Personal_development/Personal_R/fplmodels/scheduler_gw_data.R",
#   schedule = "WEEKLY", starttime = "13:00", days = "WED"
# )
# taskscheduler_delete("update_fpl_data")


