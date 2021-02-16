#' @title Individual advanced statistics
#' @description This function allows the calculation of advanced individual statistics.
#' @param df1 Should be a Data Frame that represents the individual statistics or individual defensive statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the team's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @param df3 Should be a Data Frame that represents the rival's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the following advanced statistics calculated:
#'        \itemize{
#'                 \item Player Efficiency Rating (PER)
#'                 \item Efficiency Field Goals percentage (eFG\%)
#'                 \item True shooting percentage (TS\%)
#'                 \item Three rating (3Par)
#'                 \item Free Throw rating (FTr)
#'                 \item Offensive rebounds percentage (ORB\%)
#'                 \item Defensive rebounds percentage (DRB\%)
#'                 \item Total rebounds percentage (TRB\%)
#'                 \item Assists percentage (AST\%)
#'                 \item Steal percentage (STL\%)
#'                 \item Block percentage (BLK\%)
#'                 \item Turnover percentage (TOV\%)
#'                 \item Usage percentage (USG\%)
#'         }
#'
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
#' individuals_advance_stats(df1,df2,df3)
#'
#' @export
#'

individuals_advance_stats <- function(df1,df2,df3){
  df1 <-   df1[-nrow(df1),]
  tm_poss  <- df2[1,4] - df2[1,15] / (df2[1,15] + df3[1,16]) * (df2[1,4] - df2[1,3]) * 1.07 + df2[1,21] + 0.4 * df2[1,13]
  opp_poss <- df3[1,4] - df3[1,15] / (df3[1,15] + df2[1,16]) * (df3[1,4] - df3[1,3]) * 1.07 + df3[1,21] + 0.4 * df3[1,13]
  adv_stats <- cbind(df1[1],df1[2],df1[3],df1[4])
  per <- round((df1[5] * 85.910 + df1[21] * 53.897 + df1[8] * 51.757 + df1[14] * 46.845 + df1[22] * 39.190 + df1[17] * 39.190 + df1[20] * 34.677 + df1[18] * 14.707 - df1[24] * 17.174 - (df1[9] - df1[8]) * 20.091 - (df1[6] - df1[5]) * 39.190 - df1[23] * 53.897) * (1/df1[4]),2)
  tpar <- round(df1[9] / df1[6],3)
  ftr <- round(df1[15] / df1[6],3)
  efg <- round((df1[5] + 0.5 * df1[8]) / df1[6],3)
  ts <- round(df1[25] / (2 * (df1[6] + 0.44 * df1[15])),3)
  orb <- round(100 * (df1[17] * (df2[1,2] / 5)) / (df1[4] * (df2[1,15] + df3[1,16])),2)
  drb <- round(100 * (df1[18] * (df2[1,2] / 5)) / (df1[4] * (df2[1,16] + df3[1,15])),2)
  trb <- round(100 * (df1[19] * (df2[1,2] / 5)) / (df1[4] * (df2[1,17] + df3[1,17])),2)
  ast <- round(100 * (df1[20] / (((df1[4] / (df2[1,2] / 5)) * df2[1,3]) - df1[5])),2)
  stl <- round(100 * (df1[21] * (df2[1,2] / 5)) / (df1[4] * opp_poss),2)
  blk <- round(100 * (df1[22] * (df2[1,2] / 5))/ (df1[4] * (df3[1,4] - df3[1,7])),2)
  tov <- round(100 * df1[23] / (df1[6] + 0.44 * df1[15] + df1[23]),2)
  usg <- round(100 * ((df1[6] + 0.44 * df1[15] + df1[23]) * (df2[1,2] / 5)) / (df1[4] * (df2[1,4] + 0.44 * df2[1,13] + df2[1,21])),2)
  adv_stats <- cbind(adv_stats,per,efg,ts,tpar,ftr,orb,drb,trb,ast,stl,blk,tov,usg)
  names(adv_stats) =  c("Name","G","GS","MP","PER","eFG%","TS%","3PAr","FTr","ORB%","DRB%","TRB%","AST%","STL%","BLK%","TOV%","USG%")
  adv_stats[is.na(adv_stats)] <- 0
  return(adv_stats)
}

