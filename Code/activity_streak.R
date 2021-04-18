library(data.table)
library(zoo)

get_streak <- function(dt, player, input_date, streak = 0) {
  input_date = as.Date(input_date)
  start_date = as.Date(sort(unique(dt[, game_date]))[1])
  games_played <- dt[as.Date(game_date) == input_date,]
  players_played <- unique(unlist(list(unique(games_played[, player2_id]), unique(games_played[, player1_id]))))
  if ((player %in% players_played) & (input_date > start_date)) {
    get_streak(dt, player, input_date - 1, streak + 1)
  } else {
    return(streak)
  }
}

get_longest_streak <- function(dt, player) {
  start_date = as.Date(sort(unique(dt[, game_date]))[1])
  end_date = as.Date(sort(unique(dt[, game_date]), decreasing = TRUE)[1])
  longest_streak = 0
  for (x in seq(end_date, start_date, by = -1)) {
    streak = get_streak(dt, player, x)
    if (streak > longest_streak) {
      # Can be optimized by skipping all next days already includede in the streak
      longest_streak = streak
    }
  }
  return(longest_streak)
}



############################## POC ######################################

dt <- read.csv(file = "./Data/game_simulation_data.csv")
dt <- data.table(dt)
get_streak(dt, "Luis", "2020-09-01")
get_longest_streak(dt, "Chris")
