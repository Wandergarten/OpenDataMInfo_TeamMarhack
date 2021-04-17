library(data.table)
library(magrittr)

game_data <- data.table()
#game_data[, `:=`(player1 = 0, player2 = 0, time_stamp = 0, time_to_score = 0, who_serves = 1, player_skill1 = 1, player_skill2 = 1)]

# spielerprofilProfi #1
# spielerprofilMittel #2
# spielerprofilAnfaenger #3
## spieler in abh. von spielstärker macht punkt mit bestimmter wsk // bernulli 
# Differenz=0: gleich mit gleich 50/50
# Differenz=-2/2: profi/anfänger 80/20 
# Differenz=-1/1: profi/mittel oder mittel/anfänger 60/40 

## forschleife bis ein spieler 21 (konfigurierbarer parameter) punkte hat
#######################################################################################
## Game Simulation Function
#######################################################################################
game_simulation <- function(player_skill1, player_skill2, player_to_start) {
  time_stamp = 0
  player1_score = 0
  player2_score = 0
  winning_probability = 0
  who_serves = player_to_start
  skilldifference = player_skill1 - player_skill2
  if (skilldifference == 0) winning_probability = 0.5
  if (skilldifference == 1) winning_probability = 0.4
  if (skilldifference == 2) winning_probability = 0.2
  if (skilldifference == -1) winning_probability = 0.6
  if (skilldifference == -2) winning_probability = 0.8

  while (player1_score < 21 & player2_score < 21) {
    # spielzug-dauer pro Punkt
    time_to_score <- rchisq(1, df = 1, ncp = 25)
    time_stamp <- time_stamp + time_to_score

    # wer hat den Punkt erspielt?
    tmp_player1 <- rbinom(1, 1, winning_probability)
    tmp_player2 <- ifelse(tmp_player1 == 1, 0, 1)

    player1_score = player1_score + tmp_player1
    player2_score = player2_score + tmp_player2
    new_game_data_entry <- data.table(player1 = player1_score,
                                         player2 = player2_score,
                                         time_stamp = time_stamp,
                                         time_to_score = time_to_score,
                                         who_serves = who_serves,
                                         player_skill1 = player_skill1, #todo
                                         player_skill2 = player_skill2) #todo
    who_serves <- ifelse((player1_score + player2_score) %% 5 == 0, abs(who_serves - 1), who_serves)
    game_data <- rbind(game_data, new_game_data_entry)

  }
  return(game_data)
}
###################################################################################
## Data Simulation
###################################################################################
## exemplary player profile generation
set.seed(122)
player_profiles <- data.table(skill = ceiling(runif(6, 0, 3)), player = c("Luis", "Karo", "Jonny", "Chris", "Quirin", "Otto"))


## exemplary game stats through game simulation
game_simulation_data <- data.table()

for (i in 1:1000) {
  # simulate 200 games with different player pairings.
  players <- sample(1:6, 2)

  game_simulation_data <- rbind(game_simulation_data,
                                  game_simulation(player_profiles[players[1], skill],
                                                  player_profiles[players[2], skill],
                                                  player_to_start = round(runif(1, 0, 1))) %>%
                                                   cbind(game_id = i) %>%
                                                    cbind(player1_id = player_profiles[players[1], player]) %>%
                                                    cbind(player2_id = player_profiles[players[2], player]))
}

fwrite(game_simulation_data, "./game_simulation_data.csv")

# n=punktwechsel

## spielzuglänge ist.. chi-verteilt? also normal aber nur positiv? ..
# rechtssteil und positiv. 
# 


# chris, noch da? Ja
