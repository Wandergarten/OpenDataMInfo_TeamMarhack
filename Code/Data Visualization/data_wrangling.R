library(data.table)

dt <- read.csv(file = "./Data/game_simulation_data.csv")

## wie schnell werden die spiele gewonnen:
chosen_player = c("Luis", "Karo", "Jonny", "Chris", "Quirin", "Otto")[4]

str(dt)
dt[dt["player1_id"] == chosen_player | dt["player2_id"] == chosen_player,]









