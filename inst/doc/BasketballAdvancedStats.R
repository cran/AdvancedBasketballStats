## ----setup, include=FALSE-----------------------------------------------------
library(AdvancedBasketballStats)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
individual_stats <- data.frame("Name" = c("James","Team"), "G" = c(67,0), 
"GS" = c(62,0),"MP" = c(2316,1), "FG" = c(643,0), "FGA" = c(1303,0),
"3P  " = c(148,0),"3PA" = c(425,0),"2P" = c(495,0), "2PA" = c(878,0),
"FT" = c(264,0),"FTA" = c(381,0),"ORB" = c(66,0),  "DRB" = c(459,0),
"AST" = c(684,0),"STL" = c(78,0),  "BLK" = c(36,0),"TOV" = c(261,0), 
"PF" = c(118,0),"PTS" = c(1698,0),  "+/-" = c(0,0))
  
individual_stats <- individuals_data_adjustment(individual_stats)
individual_stats

## -----------------------------------------------------------------------------
individual  <- data.frame("name" = c("LeBron James","Team"),"G" = c(67,0),
"GS" = c(62,0),"MP" = c(2316,0),"FG" = c(643,0), "FGA" = c(1303,0),
"Percentage FG" = c(0.493,0),"3P" = c(148,0),"3PA" = c(425,0),
"Percentage 3P" = c(0.348,0),"2P" = c(495,0),"2PA" = c(878,0),
"Percentage 2P" = c(0.564,0),"FT" = c(264,0),"FTA FG" = c(381,0),
"Percentage FT" = c(0.693,0), "ORB" = c(66,0),"DRB" = c(459,0),
"TRB" = c(525,0),"AST" = c(684,0),"STL" = c(78,0),"BLK" = c(36,0),
"TOV" = c(261,0), "PF" = c(118,0),"PTS" = c(1698,0),"+/-" = c(0,0))

individual

## -----------------------------------------------------------------------------
defensive_stats <- data.frame("Name" = c("Witherspoon ","Team"), 
"MP" = c(14,200),"DREB" = c(1,0),"FM" = c(4,0), "BLK" = c(0,0),
"FTO" = c(0,0),"STL" = c(1,1), "FFTA" = c(0,0),  "DFGM" = c(1,0),
"DFTM" = c(0,0))

defensive_stats <- individuals_data_adjustment(defensive_stats)
defensive_stats

## -----------------------------------------------------------------------------
defensive <- data.frame("Name" = c("Witherspoon ","Team"), 
"MP" = c(14,200),"DREB" = c(1,0), "FM" = c(4,0),
"BLK" = c(0,0),"TOTAL FM" = c(4,0),"FTO" = c(0,0),
"STL" = c(1,1), "TOTAL FTO " = c(1,0), "FFTA" = c(0,0),
"DFGM" = c(1,0), "DFTM" = c(0,0))

defensive

## -----------------------------------------------------------------------------
tm_stats <- team_stats(individual_stats)
tm_stats

## -----------------------------------------------------------------------------
indi_team_stats <- data.frame("G" = c(71), "MP" = c(17090), 
"FG" = c(3006), "FGA" = c(6269),"Percentage FG" = c(0.48),
"3P" = c(782),"3PA" = c(2242), "Percentage 3P" = c(0.349),
"2P" = c(2224), "2PA" = c(4027), "Percentage 2P" = c(0.552),
"FT" = c(1260),"FTA FG" = c(1728),"Percentage FT" = c(0.729),
"ORB" = c(757),"DRB" = c(2490),"TRB" = c(3247),"AST" = c(1803),
"STL" = c(612), "BLK" = c(468),"TOV" = c(1077),"PF" = c(1471), 
"PTS" = c(8054),  "+/-" = c(0))

indi_team_stats    

## -----------------------------------------------------------------------------
indi_rival_stats <- data.frame("G" = c(71), "MP" = c(17090), 
"FG" = c(2773), "FGA" = c(6187),"Percentage FG" = c(0.448),
"3P" = c(827),"3PA" = c(2373),"Percentage 3P" = c(0.349),
"2P" = c(1946), "2PA" = c(3814),"Percentage 2P" = c(0.510),
"FT" = c(1270),"FTA FG" = c(1626),"Percentage FT" = c(0.781),
"ORB" = c(668), "DRB" = c(2333),"TRB" = c(3001), "AST" = c(1662), 
"STL" = c(585),"BLK" = c(263),"TOV" = c(1130),"PF" = c(1544),
"PTS" = c(7643),  "+/-" = c(0))

indi_rival_stats

## -----------------------------------------------------------------------------
linp_basic <- data.frame("PG"= c("James","Rondo"),"SG" = c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(4,0),
"FGA"  = c(7,0), "X3P" = c(0,0),"X3PA" = c(2,0),"X2P" = c(4,0),
"X2PA" = c(5,0),  "FT" = c(1,0), "FTA" = c(3,0),"ORB" = c(2,0),
"DRB" = c(5,0), "AST " = c(2,0), "STL " = c(1,0), "BLK " = c(0,0),
"TOV " = c(7,2), "PF" = c(1,0),  "PLUS" = c(9,0),"MINUS" = c(17,3))

linp_basic <- lineups_data_adjustment(linp_basic)
linp_basic

## -----------------------------------------------------------------------------
lineup_basic <- data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(4,0),
"FGA " = c(7,0),"Percentage FG" = c(0.571,0),
"X3P  " = c(0,0),"X3PA  " = c(2,0),"Percentage 3P" = c(0,0),
"X2P " = c(4,0), "X2PA " = c(5,0), "Percentage 2P" = c(0.8,0),
"FT " = c(1,0), "FTA " = c(3,0), "Percentage FT" = c(0.333,0),
"ORB " = c(2,0), "DRB " = c(5,0),"TRB " = c(7,0), "AST " = c(2,0),
"STL " = c(1,0), "BLK " = c(0,0),"TOV " = c(7,2), "PF" = c(1,0),
"PLUS" = c(9,0),"MINUS" = c(17,3),"P/M" = c(-8,-3))

lineup_basic

## -----------------------------------------------------------------------------
linp_extended <-  data.frame("PG" = c("James","Rondo"),"SG"= c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(6,0),
"OppFG " = c(6,0), "FGA " = c(10,0),"OppFGA " = c(9,0),
"X3P  " = c(2,0),"Opp3P  " = c(1,0),"X3PA " = c(4,0),
"Opp3PA " = c(3,0),"X2P" = c(4,0),"Opp2P" = c(5,0),"X2PA " = c(6,0),
"Opp2PA" = c(8,0) , "FT " = c(0,0),"OppFT " = c(1,0), "FTA " = c(0,0),
"OppFTA" = c(1,0),"OppRB" = c(2,0),"OppOppRB" = c(1,0),"DRB" = c(4,0),
"OppDRB" = c(1,0),"AST " = c(5,0),"OppAST " = c(4,0),"STL" = c(1,0),
"OppSTL" = c(3,0),"BLK" = c(0,0),"OppBLK" = c(1,0),"TOppV" = c(5,2),
"OppTOppV" = c(3,2),"PF" = c(1,0),"OppPF" = c(3,0),"PLUS" = c(15,0),
"MINUS" = c(14,3))

linp_extended <- lineups_data_adjustment(linp_extended)
linp_extended

## -----------------------------------------------------------------------------
lineup_extended <-  data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(6,0),
"OppFG " = c(6,0), "FGA " = c(10,0),"OppFGA " = c(9,0),
"X3P  " = c(2,0),"Opp3P" = c(1,0),"X3PA" = c(4,0),"Opp3PA" = c(3,0),
"X2P" = c(4,0),"Opp2P " = c(5,0), "X2PA " = c(6,0),"Opp2PA " = c(8,0) ,
"FT " = c(0,0),"OppFT " = c(1,0), "FTA " = c(0,0),"OppFTA " = c(1,0),
"OppRB " = c(2,0),"OppOppRB " = c(1,0), "DRB" = c(4,0),"OppDRB" = c(1,0),
"TRB" = c(6,0),"OppTRB" = c(2,0), "AST " = c(5,0),"OppAST " = c(4,0),
"STL " = c(1,0),"OppSTL " = c(3,0), "BLK " = c(0,0),  "OppBLK " = c(1,0),
"TOppV " = c(5,2), "OppTOppV " = c(3,2),"PF" = c(1,0),"OppPF" = c(3,0),
"PLUS" = c(15,0),"MINUS" = c(14,3),"P/M" = c(1,-3))

lineup_extended

## -----------------------------------------------------------------------------
tm_stats <- team_stats(individual_stats)
tm_stats

## -----------------------------------------------------------------------------
lineup_team_stats <- data.frame("G" = c(71), "MP" = c(17090),
 "FG" = c(3006), "FGA" = c(6269),"Percentage FG" = c(0.48),
 "3P" = c(782),"3PA" = c(2242), "Percentage 3P" = c(0.349),
"2P" = c(2224), "2PA" = c(4027), "Percentage 2P" = c(0.552),
"FT" = c(1260),"FTA FG" = c(1728),  "Percentage FT" = c(0.729),
"ORB" = c(757), "DRB" = c(2490),"TRB" = c(3247),"AST" = c(1803), 
"STL" = c(612),  "BLK" = c(468),"TOV" = c(1077),"PF" = c(1471),  
"PTS" = c(8054),  "+/-" = c(0))

lineup_team_stats    

## -----------------------------------------------------------------------------
lineup_rival_stats <- data.frame("G" = c(71), "MP" = c(17090),
"FG" = c(2773), "FGA" = c(6187),"Percentage FG" = c(0.448),
"3P" = c(827),"3PA" = c(2373),"Percentage 3P" = c(0.349),
"2P" = c(1946), "2PA" = c(3814),"Percentage 2P" = c(0.510), 
"FT" = c(1270),"FTA FG" = c(1626),  "Percentage FT" = c(0.781),
"ORB" = c(668), "DRB" = c(2333),"TRB" = c(3001), "AST" = c(1662), 
"STL" = c(585),"BLK" = c(263),"TOV" = c(1130),"PF" = c(1544), 
"PTS" = c(7643),  "+/-" = c(0))

lineup_rival_stats

## -----------------------------------------------------------------------------
play_stats <- data.frame("Name" = c("Sabonis ","Team"), 
"GP" = c(62,71),"PTS" = c(387,0), "FG" = c(155,1),
"FGA" = c(281,1),"3P" = c(6,1),"3PA" = c(18,1),
"FT" = c(39,1),  "FTA" = c(53,1),"ANDONE" = c(12,1),
"AST" = c(0,1), "TOV" = c(27,1))

play_stats <- play_data_adjustment(play_stats)
play_stats

## -----------------------------------------------------------------------------
play <- data.frame("Name" = c("Sabonis ","Team"),
"GP" = c(62,71),"PTS" = c(387,0), "FG" = c(155,1),
"FGA" = c(281,1),"3P" = c(6,1),"3PA" = c(18,1), 
"FT" = c(39,1), "FTA" = c(53,1),"ANDONE" = c(12,1),
"AST" = c(0,1), "TOV" = c(27,1))

play

## -----------------------------------------------------------------------------
tm_stats <- team_stats(individual_stats)
tm_stats

## -----------------------------------------------------------------------------
play_team_stats <- data.frame("G" = c(71), "MP" = c(17090),
"FG" = c(3006), "FGA" = c(6269),"Percentage FG" = c(0.48),
"3P" = c(782),"3PA" = c(2242), "Percentage 3P" = c(0.349),
"2P" = c(2224), "2PA" = c(4027), "Percentage 2P" = c(0.552),
"FT" = c(1260),"FTA FG" = c(1728),  "Percentage FT" = c(0.729),
"ORB" = c(757),"DRB" = c(2490),"TRB" = c(3247),"AST" = c(1803),
"STL" = c(612), "BLK" = c(468),"TOV" = c(1077),"PF" = c(1471), 
"PTS" = c(8054),  "+/-" = c(0))

play_team_stats    

## -----------------------------------------------------------------------------
play_rival_stats <- data.frame("G" = c(71), "MP" = c(17090),
"FG" = c(2773), "FGA" = c(6187),"Percentage FG" = c(0.448),
"3P" = c(827),"3PA" = c(2373),"Percentage 3P" = c(0.349),
"2P" = c(1946), "2PA" = c(3814),"Percentage 2P" = c(0.510),
"FT" = c(1270),"FTA FG" = c(1626),"Percentage FT" = c(0.781),
"ORB" = c(668),"DRB" = c(2333),"TRB" = c(3001),"AST" = c(1662),
"STL" = c(585),"BLK" = c(263),"TOV" = c(1130),"PF" = c(1544),
"PTS" = c(7643),  "+/-" = c(0))

play_rival_stats

## -----------------------------------------------------------------------------
tm_stats <- team_stats(individual_stats)
tm_stats

## -----------------------------------------------------------------------------
team_stats <- data.frame("G" = c(71), "MP" = c(17090),
"FG" = c(3006), "FGA" = c(6269),"Percentage FG" = c(0.48),
"3P" = c(782),"3PA" = c(2242), "Percentage 3P" = c(0.349),
"2P" = c(2224), "2PA" = c(4027),"Percentage 2P" = c(0.552),
"FT" = c(1260),"FTA FG" = c(1728),"Percentage FT" = c(0.729),
"ORB" = c(757),"DRB" = c(2490),"TRB" = c(3247),"AST" = c(1803),
"STL" = c(612),"BLK" = c(468),"TOV" = c(1077),"PF" = c(1471),
"PTS" = c(8054),  "+/-" = c(0))

team_stats    

## -----------------------------------------------------------------------------
rival_stats <- data.frame("G" = c(71), "MP" = c(17090), 
"FG" = c(2773), "FGA" = c(6187),"Percentage FG" = c(0.448),
"3P" = c(827),"3PA" = c(2373),"Percentage 3P" = c(0.349),
"2P" = c(1946), "2PA" = c(3814),"Percentage 2P" = c(0.510),
"FT" = c(1270),"FTA FG" = c(1626),"Percentage FT" = c(0.781),
"ORB" = c(668),"DRB" = c(2333),"TRB" = c(3001),"AST" = c(1662),
"STL" = c(585),"BLK" = c(263),"TOV" = c(1130),"PF" = c(1544),
"PTS" = c(7643),  "+/-" = c(0))

rival_stats

## -----------------------------------------------------------------------------
advanced_stats <- individuals_advance_stats(individual,indi_team_stats,indi_rival_stats)
advanced_stats

## -----------------------------------------------------------------------------
defensive_actual_stats <- individuals_defensive_actual_floor_stats(defensive,indi_team_stats,indi_rival_stats)
defensive_actual_stats

## -----------------------------------------------------------------------------
defensive_estimated_stats <- individuals_defensive_estimated_floor_stats(individual,indi_team_stats,indi_rival_stats)
defensive_estimated_stats

## -----------------------------------------------------------------------------
games_adder <- individuals_games_adder(individual,individual)
games_adder

## -----------------------------------------------------------------------------
ofensive_stats <- individuals_ofensive_floor_stats(individual,indi_team_stats,indi_rival_stats)
ofensive_stats

## -----------------------------------------------------------------------------
per_game_stats <- individuals_stats_per_game(individual)
per_game_stats

## -----------------------------------------------------------------------------
per_minutes_stats <- individuals_stats_per_minutes(individual,36)
per_minutes_stats

## -----------------------------------------------------------------------------
per_poss_stats <- individuals_stats_per_possesion(individual,indi_team_stats,indi_rival_stats,100,48)
per_poss_stats

## -----------------------------------------------------------------------------
lnp_advanced_stats <- lineups_advance_stats(lineup_extended,48)
lnp_advanced_stats

## -----------------------------------------------------------------------------
lnp_bs_backcourt <- lineups_backcourt(lineup_basic)
lnp_bs_backcourt

## -----------------------------------------------------------------------------
lnp_ex_backcourt <- lineups_backcourt(lineup_extended)
lnp_ex_backcourt

## -----------------------------------------------------------------------------
lnp_comparator <- lineups_comparator_stats(lineup_extended,48)
lnp_comparator

## -----------------------------------------------------------------------------
lnp_games_adder <- lineups_games_adder(lineup_basic,lineup_basic)
lnp_games_adder

## -----------------------------------------------------------------------------
lnp_bc_paint <- lineups_paint(lineup_basic)
lnp_bc_paint

## -----------------------------------------------------------------------------
lnp_ex_paint <- lineups_paint(lineup_extended)
lnp_ex_paint

## -----------------------------------------------------------------------------
lnp_bc_players <- lineups_players(lineup_basic,5)
lnp_bc_players

## -----------------------------------------------------------------------------
lnp_ex_players <- lineups_players(lineup_extended,5)
lnp_ex_players

## -----------------------------------------------------------------------------
lnp_bc_searcher <- lineups_searcher(lineup_basic,1,"James","","","")
lnp_bc_searcher

## -----------------------------------------------------------------------------
lnp_ex_searcher <- lineups_searcher(lineup_extended,1,"James","","","")
lnp_ex_searcher

## -----------------------------------------------------------------------------
lnp_ex_sep_one <- lineups_separator(lineup_extended,1)
lnp_ex_sep_one

## -----------------------------------------------------------------------------
lnp_ex_sep_two <- lineups_separator(lineup_extended,2)
lnp_ex_sep_two

## -----------------------------------------------------------------------------
lnp_per_poss_stats <- lineups_stats_per_possesion(lineup_basic,lineup_team_stats,lineup_rival_stats,100,48)
lnp_per_poss_stats

## -----------------------------------------------------------------------------
play_adv_stats <- play_advance_stats(play_stats)
play_adv_stats

## -----------------------------------------------------------------------------
pl_game_adder <- play_games_adder(play_stats,play_stats)
pl_game_adder

## -----------------------------------------------------------------------------
play_per_game_stat <- play_stats_per_game(play_stats)
play_per_game_stat

## -----------------------------------------------------------------------------
play_per_poss_stats <- play_stats_per_possesion(play_stats,play_team_stats,play_rival_stats,100,48)
play_per_poss_stats

## -----------------------------------------------------------------------------
play_team_stats <- play_team_stats(play_stats)
play_team_stats

## -----------------------------------------------------------------------------
team_adv_stats  <- team_advanced_stats(team_stats,rival_stats,48)
team_adv_stats

## -----------------------------------------------------------------------------
team_per_game_stat <- team_stats_per_game(team_stats)
team_per_game_stat

## -----------------------------------------------------------------------------
team_per_minutes_stat <- team_stats_per_minutes(team_stats,36)
team_per_minutes_stat

## -----------------------------------------------------------------------------
team_per_poss_stat <- team_stats_per_possesion(team_stats,rival_stats,100)
team_per_poss_stat

