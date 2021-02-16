#' @title Team stats per game
#' @description The function allows the calculation of team's statistics per game.
#' @param df1 Should be a Data Frame that represents the team statistics. This parameter has to be in the format provided by the team_stats() function.
#' @details The calculation is made with the number of games played by the team.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame whit the team's statistics per game
#' @examples
#'
#' df1 <- data.frame("G" = c(71), "MP" = c(17090), "FG" = c(3006),
#' "FGA" = c(6269),"Percentage FG" = c(0.48), "3P" = c(782),
#' "3PA" = c(2242), "Percentage 3P" = c(0.349),  "2P" = c(2224),
#' "2PA" = c(4027), "Percentage 2P" = c(0.552), "FT" = c(1260),
#' "FTA FG" = c(1728),  "Percentage FT" = c(0.729), "ORB" = c(757),
#' "DRB" = c(2490),"TRB" = c(3247),  "AST" = c(1803),  "STL" = c(612),
#' "BLK" = c(468),   "TOV" = c(1077),  "PF" = c(1471),  "PTS" = c(8054),
#' "+/-" = c(0))
#'
#' team_stats_per_game(df1)
#'
#' @export
#'

team_stats_per_game <- function(df1){
  for(i in 3:ncol(df1)){
    if(i==5 || i==8 || i==11 || i==14){
      df1[i] <- round(df1[i],3)
    }
    else{
      df1[i] <- round(df1[i] / df1[1],2)
    }
  }
  names(df1) <- c("G","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                  "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
  return(df1)
}
