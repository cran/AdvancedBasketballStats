#' @title Individual's defensive actual statistics
#' @description The function allows the calculation of individual defensive actual statistics on court
#' @param df1 Should be a Data Frame that represents the individual defensive statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the team's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @param df3 Should be a Data Frame that represents the rival's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the following individual defensive actual statistics
#'        \itemize{
#'                 \item Defensive Stops (DStops)
#'                 \item Defensive Scores Possesions (DscPoss)
#'                 \item Defensive Possesions (DPoss)
#'                 \item Stops percentage (STOPS\%)
#'                 \item (TMDPossS\%)
#'                 \item Defensive Rating (DRtg)
#'         }
#' @examples
#'
#' df1 <- data.frame("Name" = c("Witherspoon ","Team"), "MP" = c(14,200),
#' "DREB" = c(1,0), "FM" = c(4,0), "BLK" = c(0,0),"TOTAL FM" = c(4,0),
#' "FTO" = c(0,0),"STL" = c(1,1), "TOTAL FTO " = c(1,0), "FFTA" = c(0,0),
#' "DFGM" = c(1,0), "DFTM" = c(0,0))
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
#' individuals_defensive_actual_floor_stats(df1,df2,df3)
#'
#' @export
#'

individuals_defensive_actual_floor_stats <- function(df1,df2,df3){
  stats <- cbind(df1[1],df1[2])
  poss  <- df2[1,4] - df2[1,15] / (df2[1,15] + df3[1,16]) * (df2[1,4] - df2[1,3]) * 1.07 + df2[1,21] + 0.4 * df2[1,13]
  ope_poss <- df3[1,4] - df3[1,15] / (df3[1,15] + df2[1,16]) * (df3[1,4] - df3[1,3]) * 1.07 + df3[1,21] + 0.4 * df3[1,13]
  team_possessions <- (poss+ope_poss)/2
  dor <- df3[1,15] / (df3[1,15] + df2[1,16])
  dfg <- df3[1,3] / df3[1,4]
  fmwt <- (dfg * (1 - dor)) / (dfg * (1 - dor) + (1 - dfg) * dor)
  dstops <- df1[7] + df1[8] + df1[10]/10 + (df1[4] + df1[5]) * fmwt * (1-dor) + df1[3] * (1-fmwt)
  aux <- 0.45 * (df1[10] + df1[12]) * (1-(1- df1[12]/(df1[10]+df1[12])))^2
  aux[is.na(aux)] <- 0
  dscposs <- df1[11] + aux
  dposs <- dstops + dscposs
  stops <- dstops / (dstops + dscposs)
  tmdposs <- (40/df1[2])*dposs/team_possessions
  team_defensive_rating <- 100 * (df3[1,23] / team_possessions)
  d_pts_per_scposs <- df3[1,23] / (df3[1,3] + (1 - (1 - (df3[1,12] / df3[1,13]))^2) * df3[1,13]*0.4)
  drtg <- team_defensive_rating + tmdposs * (100 * d_pts_per_scposs * (1-stops) - team_defensive_rating)
  stats <- cbind(stats,round(dstops,2),round(dscposs,2),round(dposs,2),round(stops,3),round(tmdposs,3),round(drtg,2))
  names(stats) =  c("Name","MP","DStops","DScPoss","DPoss","Stop%","TMDPoss%","DRtg")
  stats[is.na(stats)] <- 0
  return(stats)
}
