#' @title Team play statistics
#' @description The function performs the sum of the statistical sections to obtain a data.frame with the total statistics of the team.
#' @param df1 Should be a Data Frame that represents the play's statistics. This parameter has to be in the format provided by the play_data_adjustment() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame whit the team's plays statistics
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
#' play_team_stats(df1)
#'
#' @export
#'

play_team_stats <- function(df1){
  total <- data.frame("G" = df1[nrow(df1),2])
  for(i in 3:ncol(df1)){
    if(i==6 || i==9 || i==12 || i==15){
      div <- colSums(df1[i-2])/colSums(df1[i-1])
      total <- cbind(total,round(div,3))
    }
    else{
      total <- cbind(total,round(colSums(df1[i]),3))
    }
  }
  total <- data.frame(total, row.names = 1)
  total[is.na(total)] <- 0
  names(total) <- c("GP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%","And One","AST","TOV")
  return(total)
}

