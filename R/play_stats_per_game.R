#' @title Play stats per game
#' @description The function allows the calculation of play statistics per game.
#' @param df1 Should be a Data Frame that represents the play's statistics. The parameter has to be in the format provided by the play_data_adjustment() function.
#' @details The calculation is made with the number of games played by the player.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with play statistics per game
#' @examples
#'
#' df1 <- data.frame("Name" = c("Sabonis ","Team"), "GP" = c(62,71),
#' "PTS" = c(387,0), "FG" = c(155,1), "FGA" = c(281,1),
#' "FGA Percentage" = c(0.552,1),"3P" = c(6,1),"3PA" = c(18,1),
#' "3P Percentage" = c(0.333,1),"2P" = c(149,0),"2PA" = c(263,0),
#' "2P Percentage" = c(0.567,0),"FT" = c(39,1),  "FTA" = c(53,1),
#' "FT Percentage" = c(0.736,1),  "ANDONE" = c(12,1), "AST" = c(0,1),
#' "TOV" = c(27,1))
#'
#' play_stats_per_game(df1)
#'
#' @export
#'

play_stats_per_game <- function(df1){
      df1 <- df1[-nrow(df1),]
      for(i in 3:ncol(df1)){
        if(i==6 || i==9 || i==12 || i==15){
          df1[i] <- round(df1[i],3)
        }
        else{
          df1[i] <- round(df1[i] / df1[2],2)
        }
      }
      names(df1) <- c("Name","GP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%","And One","AST","TOV")
      df1[is.na(df1)] <- 0

  return(df1)
}
