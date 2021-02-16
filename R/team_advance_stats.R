#' @title Team advanced statistics
#' @description This function allows the calculation of advanced team's statistics.
#' @param df1 Should be a Data Frame that represents the team statistics. This parameter has to be in the format provided by the team_stats() function.
#' @param df2 Should be a Data Frame that represents the rival statistics. This parameter has to be in the format provided by the team_stats() function.
#' @param m should be a number. This parameter has to be the duration of a single game.
#' @return Data frame with the following advanced statistics calculated:
#'        \itemize{
#'                 \item Offensive Rating (ORtg)
#'                 \item Defensive Rating (DRtg)
#'                 \item Net Rating (NetRtg)
#'                 \item Pace (Pace)
#'                 \item Three rating (3Par)
#'                 \item True shooting percentage (TS\%)
#'                 \item Efficiency Field Goals percentage (eFG\%)
#'                 \item Assists percentage (AST\%)
#'                 \item Assist to Turnover Ratio (AST/TO)
#'                 \item Assist Ratio (ASTRATIO)
#'                 \item Offensive rebounds percentage (ORB\%)
#'                 \item Defensive rebounds percentage (DRB\%)
#'                 \item Total rebounds percentage (TRB\%)
#'                 \item Turnover percentage (TOV\%)
#'                 \item Free Throw rating (FTr)
#'                 \item Opponent Efficiency Field Goals percentage (Opp eFG\%)
#'                 \item Opponent Turnover percentage (Opp TOV\%)
#'                 \item Opponent Defensive rebounds percentage (Opp DRB\%)
#'                 \item Opponent Free Throw rating (Opp FTr)
#'         }
#' @examples
#' df1 <- data.frame("G" = c(71), "MP" = c(17090), "FG" = c(3006),
#' "FGA" = c(6269),"Percentage FG" = c(0.48), "3P" = c(782),
#' "3PA" = c(2242), "Percentage 3P" = c(0.349),  "2P" = c(2224),
#' "2PA" = c(4027), "Percentage 2P" = c(0.552), "FT" = c(1260),
#' "FTA FG" = c(1728),  "Percentage FT" = c(0.729), "ORB" = c(757),
#' "DRB" = c(2490),"TRB" = c(3247),"AST" = c(1803),"STL" = c(612),
#' "BLK" = c(468),   "TOV" = c(1077),  "PF" = c(1471),
#' "PTS" = c(8054),  "+/-" = c(0))
#'
#' df2 <- data.frame("G" = c(71), "MP" = c(17090), "FG" = c(2773),
#' "FGA" = c(6187),"Percentage FG" = c(0.448), "3P" = c(827),
#' "3PA" = c(2373), "Percentage 3P" = c(0.349),  "2P" = c(1946),
#' "2PA" = c(3814), "Percentage 2P" = c(0.510), "FT" = c(1270),
#' "FTA FG" = c(1626),  "Percentage FT" = c(0.781), "ORB" = c(668),
#' "DRB" = c(2333),"TRB" = c(3001),  "AST" = c(1662),"STL" = c(585),
#' "BLK" = c(263),   "TOV" = c(1130),  "PF" = c(1544),
#' "PTS" = c(7643),  "+/-" = c(0))
#'
#' m <- 48
#'
#' team_advanced_stats(df1,df2,m)
#'
#' @export
#'

team_advanced_stats <- function(df1,df2,m){
  minutes <- (df2[1,2]/df2[1,1])/5
  minutes <- trunc(minutes)
  tm_poss  <- df1[1,4] - df1[1,15] / (df1[1,15] + df2[1,16]) * (df1[1,4] - df1[1,3]) * 1.07 + df1[1,21] + 0.4 * df1[1,13]
  opp_poss <- df2[1,4] - df2[1,15] / (df2[1,15] + df1[1,16]) * (df2[1,4] - df2[1,3]) * 1.07 + df2[1,21] + 0.4 * df2[1,13]
  pace     <- round(m * ((tm_poss + opp_poss) / (2 * (df1[1,2] / 5))),2)
  stats <- cbind(df1[1],df1[2])
  offrtg <- round(100 * (df1[1,23]/tm_poss),2)
  defrtg <- round(100 * (df2[1,23]/opp_poss),2)
  netrtg <- round(offrtg - defrtg,2)
  TPAr <- round(df1[1,7] / df1[1,4],3)
  FTr <- round(df1[1,13] / df1[1,4],3)
  ts <- round(df1[1,23] / (2 * (df1[1,4] + 0.44 * df1[1,13])),3)
  efg <- round((df1[1,3] + 0.5 * df1[1,6]) / df1[1,4],3)
  ast <- round(df1[1,18] / df1[1,3],3)
  ast_to <- round(df1[1,18] / df1[1,21],2)
  ast_ratio <- round((df1[1,18] * 100) / tm_poss,2)
  orb <- round(df1[1,15] / (df1[1,15] + df2[1,16]),3)
  fta_rate <- round(df1[1,12] / df1[1,4],3)
  tov <- round(df1[1,21] / (df1[1,4] +0.44 * df1[1,13] + df1[1,21]),3)
  eFG_opp <- round((df2[1,3] + 0.5 * df2[1,6]) / df2[1,4],3)
  tov_opp <- round(df2[1,21] / (df2[1,4] +0.44 * df2[1,13] + df2[1,21]),3)
  offRB_opp <- round(df1[1,16] / (df1[1,16] + df2[1,15]),3)
  fta_rate_opp <- round(df2[1,12] / df2[1,4],3)
  stats <- cbind(stats,offrtg,defrtg,netrtg,pace,TPAr,FTr,ts,efg,ast,ast_to,ast_ratio,orb,tov,fta_rate,eFG_opp,tov_opp,offRB_opp,fta_rate_opp)
  names(stats) =  c("G","MP","ORtg","DRtg",'NetRtg','Pace',"3PAr","FTr",'TS%','eFG%',"AST%","AST/TO","ASTRATIO%","ORB%","TOV%","FT/FGA","Opp eFG%","Opp TOV%","DRB%","Opp FTr")
  return(stats)
}

