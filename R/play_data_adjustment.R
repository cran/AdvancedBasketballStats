#' @title Play data adjustment
#' @description The function transform the statistics entered for later use in the rest of the functions that apply to play statistics.
#' @param df1 Should be a Data Frame that represents the play's statistics. The parameter has to be in the format provided by the play_data_adjustment() function.
#' @details \itemize{
#'                 \item The data.frame must have the same columns and these represent the same as in the example.
#'                 \item The input data.frame must have the last row that represents the team's statistics.
#'                 \item The function allows the transformation of the play statistics to which the shooting percentages.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return The data frame obtained for the play statistics will have the following format:
#'         \itemize{
#'                 \item Name of the player (Name)
#'                 \item Games Started (GS)
#'                 \item Points (PTS)
#'                 \item Field Goals Made (FG)
#'                 \item Field Goals Attempted (FGA)
#'                 \item Field Goals Percentage (FG\%)
#'                 \item Three Points Made (3P)
#'                 \item Three Points Attempted (3PA)
#'                 \item Three Points Percentage (3P\%)
#'                 \item Two Points Made (2P)
#'                 \item Two Points Attempted (2PA)
#'                 \item Two Points Percentage (2P\%)
#'                 \item Free Throw Made (FT)
#'                 \item Free Throw Attempted (FTA)
#'                 \item Free Throw Percentage (FT\%)
#'                 \item And One Times (ANDONE)
#'                 \item Assists (AST)
#'                 \item Turnover (TOV)
#'         }
#' @examples
#'
#' df1 <- data.frame("Name" = c("Sabonis ","Team"), "GP" = c(62,71),
#' "PTS" = c(387,0), "FG" = c(155,1), "FGA" = c(281,1),
#' "3P" = c(6,1),"3PA" = c(18,1), "FT" = c(39,1),  "FTA" = c(53,1),
#' "ANDONE" = c(12,1), "AST" = c(0,1), "TOV" = c(27,1))
#'
#' play_data_adjustment(df1)
#'
#' @export
#'

play_data_adjustment <- function(df1){
  if(ncol(df1)==12){
    names(df1) <- c("Name","GP","PTS","FG","FGA","3P","3PA","FT","FTA","ANDONE","AST","TOV")
    fgp <- round(df1[4] / df1[5],3)
    tp <- round(df1[6] / df1[7],3)
    tw <- round((df1[4] - df1[6]),3)
    twa <- round((df1[5] - df1[7]),3)
    twp <- round(tw / twa,3)
    ftp <- round(df1[8] / df1[9],3)
    data_adjustment <- cbind(df1,fgp,tp,tw,twa,twp,ftp)
    data_adjustment <- subset (data_adjustment, select=c(1,2,3,4,5,13,6,7,14,15,16,17,8,9,18,10,11,12))
    names(data_adjustment) <- c("Name","GP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%","ANDONE","AST","TOV")
    data_adjustment[is.na(data_adjustment)] <- 0
  }
  data_adjustment[is.na(data_adjustment)] <- 0
  return(data_adjustment)
}
