library(data.table)

game_data <- data.table()
game_data[,`:=`(player1=0, player2=0, time_stamp=0, time_to_score=0, who_serves=1, player_skill1=1, player_skill2=1)]

if(point){
    rbind(game_data, data.table())
}

spielerprofilProfi      #1
spielerprofilAnfaenger  #3
spielerprofilMittel     #2
## spieler in abh. von spielstärker macht punkt mit bestimmter wsk // bernulli 
## 6 paarungen jeder mit jedem
# Differenz=0: gleich mit gleich 50/50 --> 0.5 wsk einen Punkt für (stärkeren) spieler1 zu erzielen
# Differenz=-2 profi anfänger 80/20  --> 0.8
# Differenz=-1 profi mittel 60/40 --> 0.6
# Differenz=-1 mittel anfänger 60/40 

## forschleife bis ein spieler 21 (konfigurierbarer parameter) punkte hat

game_simulation <- function(player_skill1, player_skill2){
    time_stamp = 0
    player1_score = 0
    player2_score = 0
    skilldifference = player_skill1 - player_skill2
    skilldifference/2
    winning_probability = 

    while(player1_score < 21 & player2_score < 21){
        # spielzug-dauer pro Punkt
        time_to_score <- rchisq(1,df=1, ncp=25)
        time_stamp <- time_stamp + time_to_score

        # wer hat den Punkt erspielt?
        tmp_player1 <- rbinom(1, 1, winning_probability)
        tmp_player2 <- ifelse(tmp_player1==1, 0, 1) 

        player1_score = player1_score + tmp_player1 
        player2_score = player2_score + tmp_player2
        new_game_data_entry <-  data.table(player1=player1_score,
                                        player2=player2_score,
                                        time_stamp=time_stamp,
                                        time_to_score=time_to_score,
                                        who_serves=1, #todo
                                        player_skill1=1, #todo
                                        player_skill2=1) #todo
        game_data <- rbind(game_data, new_game_data_entry)

    }

}


ceiling(runif(50,0,3))


# n=punktwechsel

## spielzuglänge ist.. chi-verteilt? also normal aber nur positiv? ..
# rechtssteil und positiv. 
# 


# chris, noch da?
