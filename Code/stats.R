library(data.table)

final_timestamp <- function(y) {
  return(max(y$player1, y$player2) == 21)
}
calc_stats <- function(dt) {

  players <- unique(dt[, player1_id])
  num_players = length(players)
  stats <- matrix(0L, nrow = num_players, ncol = num_players)
  colnames(stats) <- players
  rownames(stats) <- players

  for (i in 1:nrow(dt)) {
    #nrow(dt)
    y = dt[i,]
    if (final_timestamp(y)) {
      # Match is over
      player_1 = match(y$player1_id, players)
      player_2 = match(y$player2_id, players)
      # Determine winner
      if (y$player1 > y$player2) {
        winner = y$player1
        loser = y$player2
        stats[player_1, player_2] <- stats[player_1, player_2] + 1
        stats[player_2, player_1] <- stats[player_2, player_1] - 1
      } else {
        winner = y$player2
        loser = y$player1
        stats[player_2, player_1] <- stats[player_2, player_1] + 1
        stats[player_1, player_2] <- stats[player_1, player_2] - 1
      }

    }
  }
  return(stats)
}

get_weakest_opponent <- function(stats, player) {
  weakest_opponent <- sort(stats[player,], decreasing = TRUE)[1]
  if (names(weakest_opponent) == player) {
    weakest_opponent <- sort(stats[player,], decreasing = TRUE)[2]
  }
  return(weakest_opponent)
}
get_strongest_opponent <- function(stats, player) {
  strongest_opponent <- sort(stats[player,])[1]
  if (names(strongest_opponent) == player) {
    strongest_opponent <- sort(stats[player,])[2]
  }
  return(strongest_opponent)
}

######################## POC ###################################
dt <- read.csv(file = "./Data/game_simulation_data.csv")
dt <- data.table(dt)
stats <- calc_stats(dt)
player = "Karo"
get_weakest_opponent(stats, player)
get_strongest_opponent(stats, player)