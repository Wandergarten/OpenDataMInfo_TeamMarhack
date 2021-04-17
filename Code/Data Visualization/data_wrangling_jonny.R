library(data.table)
library(magrittr)
library(ggplot2)
dt <- read.csv(file = "./Universität Leipzig/Marhack/OpenDataMInfo_TeamMarhack/Data/game_simulation_data.csv")
dt <- data.table(dt)
dt$game_date <- as.Date(dt[,game_date])

chosen_player = c("Luis", "Karo", "Jonny", "Chris", "Quirin", "Otto")[4]

# Q1: how fast do you win games: ----

# extract all games in which you played
chosen_player_dt <- dt[player1_id == chosen_player | player2_id == chosen_player]

# extract the games you won
winnings <- chosen_player_dt[(player1>=21 & player1_id == chosen_player) | (player2>=21 & player2_id == chosen_player)]

# the game length of won games is just the 'final' time_stamp_value.

ggplot(melt(winnings, measure.vars = c('time_stamp', 'game_date'), id.vars = 'game_id'))+
  

## wie lang sind die winstreaks:

endscores <- chosen_player_dt[(player1>=21) | (player2>=21)]

current_win_streak <- 0
current_losing_streak <- 0
winning_streak_vec <- c()
losing_streak_vec <- c()
for(i in 1:nrow(endscores)){
  if(
    (endscores$player1_id[i] == chosen_player && endscores$player1[i]==21) |
    (endscores$player2_id[i] == chosen_player && endscores$player2[i]==21)
    ){
    current_win_streak = current_win_streak+1
  }
  else{
    winning_streak_vec <- c(winning_streak_vec, current_win_streak)
    current_win_streak = 0
  }
  
  
  if(
    (endscores$player1_id[i] == chosen_player && endscores$player2[i]==21) |
    (endscores$player2_id[i] == chosen_player && endscores$player1[i]==21)
    ){
    current_losing_streak = current_losing_streak+1
  }
  else{
    losing_streak_vec <- c(losing_streak_vec, current_losing_streak)
    current_losing_streak = 0
  }
}

# concentration/spielfluss (wie lang von punkt zu punkt) 
chosen_player_dt[, time_to_score]

# spielgewinner identifizieren

dt[,winner := ifelse(max(player1)==21, player1_id, player2_id), by = game_id]

## ausgespielte gesamtpunktzahl // knappheit der siege
dt[, sum(winner==chosen_player),by = game_id]

# punktratio for chosen player.

endscores[,winner := ifelse(max(player1)==21, player1_id, player2_id), by = game_id]
endscores[, ratio:= ifelse((player1_id==chosen_player) & (winner == chosen_player), player1/player2, player2/player1)]


# on-serve-strengh
on_serve_strengh <- chosen_player_dt[chosen_player==player1_id & who_serves==1, sum(who_serves==who_scores)/sum(who_serves),by=game_id] %>% rbind(
                      chosen_player_dt[chosen_player==player2_id & who_serves==2, sum(who_serves==who_scores)/sum(who_serves-1),by=game_id]) # who_serves-1 because who_serves is 2 and we are looking for the ratio.
on_serve_strengh[order(game_id),]
setnames(on_serve_strengh,old = "V1", new = "serve_points_ratio")

# on-defense-strengh
on_defense_strengh <- chosen_player_dt[chosen_player==player1_id & who_serves==2, sum(who_serves!=who_scores)/sum(who_serves-1),by=game_id] %>% rbind(
                        chosen_player_dt[chosen_player==player2_id & who_serves==1, sum(who_serves!=who_scores)/sum(who_serves),by=game_id]) # who_serves-1 because who_serves is 2 and we are looking for the ratio.
on_defense_strengh[order(game_id),]
setnames(on_defense_strengh,old = "V1", new = "def_points_ratio")

endscores <- endscores[on_serve_strengh,on='game_id']
endscores <- endscores[on_defense_strengh,on='game_id']


## spiderplot 


## time series



