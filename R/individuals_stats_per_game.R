#' @title individual's statistics per game
#' @description The function allows the calculation of individual statistics per game.
#' @param df1 Should be a Data Frame that represents the individual statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @details The calculation is made with the number of games played by the player.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with individual statistics per game
#' @examples
#'
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
#' individuals_stats_per_game(df1)
#'
#'
#' @export
#'

individuals_stats_per_game <- function(df1){
  df1 <- df1[-nrow(df1),]
  for(i in 4:ncol(df1)){
    if(i==7 || i==10 || i==13 || i==16){
      df1[i] <- round(df1[i],3)
    }
    else{
      df1[i] <- round(df1[i] / df1[2],2)
    }
  }
  names(df1) <- c("Name","G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
               "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
  df1[is.na(df1)] <- 0
  return(df1)
}
