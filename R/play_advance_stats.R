#' @title Play advanced statistics
#' @description This function allows the calculation of advanced play statistics.
#' @param df1 Should be a Data Frame that represents the play's statistics. The parameter has to be in the format provided by the play_data_adjustment() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the following advanced statistics calculated:
#'        \itemize{
#'                 \item Points Per Possession (PPP)
#'                 \item Possessions (POSS)
#'                 \item Frequency (Freq)
#'                 \item Efficiency Field Goals percentage (eFG\%)
#'                 \item Free Throw Percentage (FT\%)
#'                 \item Assists percentage (AST\%)
#'                 \item Turnover percentage (TOV\%)
#'                 \item And One percentage (AndOne\%)
#'                 \item Score percentage (Score\%)
#'            }
#' @examples
#'
#' df1 <- data.frame("Name" = c("Sabonis ","Team"), "GP" = c(62,71),
#' "PTS" = c(387,0), "FG" = c(155,1), "FGA" = c(281,1),
#' "FGA Percentage" = c(0.552,1),"3P" = c(6,1),"3PA" = c(18,1),
#' "3P Percentage" = c(0.333,1),"2P" = c(149,0),"2PA" = c(263,0),
#' "2P Percentage" = c(0.567,0),"FT" = c(39,1),  "FTA" = c(53,1),
#' "FT Percentage" = c(0.736,1),  "ANDONE" = c(12,1), "AST" = c(0,1),
#'  "TOV" = c(27,1))
#'
#' play_advance_stats(df1)
#'
#'
#' @export
#'

play_advance_stats <- function(df1){
  if(ncol(df1)==18){
  df1 <- df1[-nrow(df1),]
  adv_stats <-df1[1:2]
  poss <- round(df1[5] + df1[18] + 0.4 * df1[14],0)
  freq <- round(poss / cumsum(poss),3)
  ppp <- round(df1[3] / poss,2)
  efg <- round((df1[4] + 0.5 * df1[7]) / df1[5],3)
  ft_freq <- round(df1[14] / poss,3)
  ast_freq <- round(df1[17] / poss,3)
  tov_freq <- round(df1[18] / poss,3)
  andone_freq <- round(df1[16] / poss,3)
  score_freq <- round((df1[4] + 0.4 * df1[13]) / poss,3)
  adv_stats <- cbind(adv_stats,poss,freq,ppp,df1[3:12],efg,ft_freq,ast_freq,tov_freq,andone_freq,score_freq)
  names(adv_stats) =  c("Name","G","Poss","Freq","PPP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","eFG%",
                        "FT%","AST%","TOV%","And One%","Score%")
  adv_stats[is.na(adv_stats)] <- 0
  }
  else if (ncol(df1)==17){
    adv_stats <-df1[2]
    poss <- round(df1[4] + df1[17] + 0.4 * df1[13],0)
    freq <- round(poss / cumsum(poss),3)
    ppp <- round(df1[2] / poss,2)
    efg <- round((df1[3] + 0.5 * df1[6]) / df1[4],3)
    ft_freq <- round(df1[13] / poss,3)
    ast_freq <- round(df1[16] / poss,3)
    tov_freq <- round(df1[17] / poss,3)
    andone_freq <- round(df1[15] / poss,3)
    score_freq <- round((df1[3] + 0.4 * df1[12]) / poss,3)
    adv_stats <- cbind(adv_stats,poss,freq,ppp,df1[2:11],efg,ft_freq,ast_freq,tov_freq,andone_freq,score_freq)
    names(adv_stats) =  c("G","Poss","Freq","PPP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","eFG%",
                          "FT%","AST%","TOV%","And One%","Score%")
  }
  return(adv_stats)
}

