library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(plotly)

dt <- data.table(read.csv(file = "game_simulation_data.csv"))
#wich player wins
#dt[,winner := ifelse(max(player1)==21, player1_id, player2_id), by = game_id]
#dt[(player1==21 | player2==21), mean_duration_player1 := mean(time_stamp), by = player1_id]
#dt[(player1==21 | player2==21), mean_duration_player2 := mean(time_stamp), by = player2_id]

players = c("Luis", "Karo", "Jonny", "Chris", "Quirin", "Otto")
player_stat <- data.table()
###############################################################################
# How many games did a player win and loose
###############################################################################
for(i in 1:6){
  games_won <- nrow(dt[(player1_id==players[i] & player1==21) | (player2_id==players[i] & player2==21)])
  games_lost <- nrow(dt[(player1_id==players[i] & player2==21) | (player2_id==players[i] & player1==21)])
  new_entry <- data.table(player=players[i], 
                                    games_won=games_won, 
                                    games_lost=games_lost)
  player_stat <- rbind(player_stat, new_entry)
}

#plot
win_loose_melt <- melt(data = player_stat[,c(1:3)], id.vars = 'player')
data_sorted <- arrange(win_loose_melt, desc(variable), player) 
data_cumsum <- ddply(data_sorted, "player", transform, label_ypos=cumsum(value))
ggplot(data_cumsum, aes(x = player, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', width = 0.6) + 
  geom_text(aes(y = label_ypos, label = value), vjust = 1.6, color = 'white', size = 3.5) +
  scale_fill_manual(values=c('#66CC99','#CC6666')) +
  guides(fill=guide_legend(title=NULL)) +
  labs(title = "Plot of won and lost games", x = "Player", y = "Number of Games") + 
  theme_classic()
###############################################################################
# mean number of game duration
###############################################################################
mean_duration <- data.table()
for (i in 1:length(players)) {
  time_stamps <- dt[(player1==21 | player2==21) & (player1_id==players[i] | player2_id==players[i])]
  mean_tmp <- round((sum(time_stamps[,3])/nrow(time_stamps))/60, digits = 2)
  mean_duration <- rbind(mean_duration, mean_tmp)
}
player_stat <- cbind(player_stat, mean_duration = mean_duration)
names(player_stat)[4] <- "mean_duration"

#plot
mean_duration_melt <- melt(data = player_stat[,c(1,4)], id.vars = 'player')
ggplot(mean_duration_melt, aes(x = player, y = value)) + 
  geom_bar(stat = 'identity', width=0.6, fill="steelblue") + 
  labs(title = "Plot of the mean duration of one game in minutes", x = "Player", y = "Mean Duration") + 
  geom_text(aes(label = value), vjust=1.6, color="white") +
  theme_classic()
###############################################################################
# number of rounds per game
###############################################################################
#dt[,rounds_per_game := .N, by = list(game_id, game_date)]

###############################################################################
# total number of games per day
###############################################################################
#dt[(player1==21 | player2==21), games_per_day := .N, by = C(player1_id)]

###############################################################################
# number of games per couple
###############################################################################
#compute number of games playes by two people
result <- c()
for (i in 1:length(players)) {
  for (j in 1:length(players)) {
    new_result <- nrow(dt[(player1==21 | player2==21) & ((player1_id==players[i] & player2_id==players[j]) | (player2_id==players[i] & player1_id==players[j]))])
    result <- append(result, new_result)
  }
}
love_matrix <- matrix(result, byrow = TRUE, nrow = length(players))
colnames(love_matrix) <- players
rownames(love_matrix) <- players

#plot it
love_melt <- melt(love_matrix)
names(love_melt) <- c("Player_1", "Player_2", "Amount")
love_plot <- ggplot(love_melt, aes(x = Player_1, y = Player_2)) +
  geom_raster(aes(fill = Amount)) +
  geom_text(aes(label = Amount), color = "white") +
  scale_fill_gradient(low="white", high="#660000") +
  labs(x="", y="", title="Who played with whom") +
  theme_bw()
ggplotly(love_plot)
###############################################################################
# number of games per Session/Day
###############################################################################
games_per_day <- data.table()
for (i in 1:length(players)) {
  date_data <- dt[(player1==21 | player2==21) & (player1_id==players[i] | player2_id==players[i]), 
                  games_per_date:=.N, by = game_date][games_per_date!="NA"]
  games_per_day <- rbind(games_per_day, mean(date_data[,games_per_date]))
}
player_stat <- cbind(player_stat, games_per_day = games_per_day)
names(player_stat)[5] <- "games_per_day"

