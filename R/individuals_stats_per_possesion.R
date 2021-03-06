#' @title individual statistics calculator per possessions
#' @description The function allows the calculation of the statistics per game projected to P possesions.
#' @param df1 Should be a Data Frame that represents the individual statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the team's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @param df3 Should be a Data Frame that represents the rival's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @param p Should be a  number. This parameter has to be the number of possessions to which you want to project the statistics.
#' @param m should be a number. This parameter has to be the duration of a single game.
#' @details The statistical projection is made from the estimation of the possessions that the team plays when the player is on the court.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with statistics by game projected to the possessions entered
#' @examples
#'
#' df1 <- data.frame("name" = c("LeBron James","Team"),"G" = c(67,0),
#' "GS" = c(62,0),"MP" = c(2316,0),"FG" = c(643,0), "FGA" = c(1303,0),
#' "Percentage FG" = c(0.493,0),"3P" = c(148,0),"3PA" = c(425,0),
#' "Percentage 3P" = c(0.348,0),"2P" = c(495,0),"2PA" = c(878,0),
#' "Percentage 2P" = c(0.564,0),"FT" = c(264,0),"FTA FG" = c(381,0),
#' "Percentage FT" = c(0.693,0), "ORB" = c(66,0),"DRB" = c(459,0),
#' "TRB" = c(525,0),"AST" = c(684,0),"STL" = c(78,0),"BLK" = c(36,0),
#' "TOV" = c(261,0),"PF" = c(118,0),"PTS" = c(1698,0),"+/-" = c(0,0))
#'
#' df2 <- data.frame("G" = c(71), "MP" = c(17090), "FG" = c(3006),
#' "FGA" = c(6269),"Percentage FG" = c(0.48),"3P" = c(782),"3PA" = c(2242),
#' "Percentage 3P" = c(0.349),"2P" = c(2224), "2PA" = c(4027),
#' "Percentage 2P" = c(0.552),"FT" = c(1260),"FTA FG" = c(1728),
#' "Percentage FT" = c(0.729), "ORB" = c(757),  "DRB" = c(2490),
#' "TRB" = c(3247), "AST" = c(1803),  "STL" = c(612),"BLK" = c(468),
#' "TOV" = c(1077),"PF" = c(1471),  "PTS" = c(8054),  "+/-" = c(0))
#'
#' df3 <- data.frame("G" = c(71), "MP" = c(17090), "FG" = c(2773),
#' "FGA" = c(6187),"Percentage FG" = c(0.448), "3P" = c(827),
#' "3PA" = c(2373), "Percentage 3P" = c(0.349),  "2P" = c(1946),
#' "2PA" = c(3814), "Percentage 2P" = c(0.510), "FT" = c(1270),
#' "FTA FG" = c(1626),  "Percentage FT" = c(0.781), "ORB" = c(668),
#' "DRB" = c(2333),"TRB" = c(3001),  "AST" = c(1662),"STL" = c(585),
#' "BLK" = c(263),   "TOV" = c(1130),  "PF" = c(1544),
#' "PTS" = c(7643),  "+/-" = c(0))
#'
#'
#' p <- 100
#'
#' m <- 48
#'
#' individuals_stats_per_possesion(df1,df2,df3,p,m)
#'
#' @export
#'

individuals_stats_per_possesion <- function(df1,df2,df3,p,m){
  df1 <- df1[-nrow(df1),]
  tm_poss  <- df2[1,4] - df2[1,15] / (df2[1,15] + df3[1,16]) * (df2[1,4] - df2[1,3]) * 1.07 + df2[1,21] + 0.4 * df2[1,13]
  opp_poss <- df3[1,4] - df3[1,15] / (df3[1,15] + df2[1,16]) * (df3[1,4] - df3[1,3]) * 1.07 + df3[1,21] + 0.4 * df3[1,13]
  pace  <- m * ((tm_poss + opp_poss) / (2 * (df2[1,2] / 5)))
  player_poss <- (pace/m) * df1[4]
  for(i in 5:ncol(df1)){
    if(i==7 || i==10 || i==13 || i==16){
      df1[i]<- round(df1[i],3)
    }
    else{
      df1[i] <- round((df1[i]/player_poss) * p,2)
    }
  }
  names(df1) <- c("Name","G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                  "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
  df1[is.na(df1)] <- 0
  return(df1)
}


