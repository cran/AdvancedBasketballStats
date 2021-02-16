#' @title individual's defensive estimated statistics
#' @description The function allows the calculation of individual defensive estimated statistics on court
#' @param df1 Should be a Data Frame. The parameter has to be in the format provided by the data_adjustment() function.
#' @param df2 Should be a Data Frame. The parameter has to be in the format provided by the team_stats() function.
#' @param df3 Should be a Data Frame. The parameter has to be in the format provided by the team_stats() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the following individual defensive estimated statistics
#'        \itemize{
#'                 \item Defensive Stops (DStops)
#'                 \item Stops percentage (STOPS\%)
#'                 \item floor percentage (Floor\%)
#'                 \item Defensive Rating (DRtg)
#'         }
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
#' individuals_defensive_estimated_floor_stats(df1,df2,df3)
#'
#' @export
#'

individuals_defensive_estimated_floor_stats <- function(df1,df2,df3){
   df1 <-   df1[-nrow(df1),]
   stats <- cbind(df1[1],df1[2])
   team_possessions  <- df2[1,4] - df2[1,15] / (df2[1,15] + df3[1,16]) * (df2[1,4] - df2[1,3]) * 1.07 + df2[1,21] + 0.4 * df2[1,13]
   dor <- df3[1,15] / (df3[1,15] + df2[1,16])
   dfg <- df3[1,3] / df3[1,4]
   fmwt <- (dfg * (1 - dor)) / (dfg * (1 - dor) + (1 - dfg) * dor)
   stops1 <- df1[21] + df1[22] * fmwt * (1 - 1.07 * dor) + df1[18] * (1 - fmwt)
   stops2 <- (((df3[1,4] - df3[1,3] - df2[1,20]) / df2[1,2]) * fmwt * (1 - 1.07 * dor) + ((df3[1,21] - df2[1,19]) / df2[1,2])) * df1[4] + (df1[24] / df2[1,22]) * 0.4 * df3[1,13] * (1 - (df3[1,12] / df3[1,13]))^2
   stops <- stops1 + stops2
   stop <- (stops * df3[1,2]) / (team_possessions * df1[4])
   stop[1][is.na(stop[1])] <- 0
   team_defensive_rating <- 100 * (df3[1,23] / team_possessions)
   d_pts_per_scposs <- df3[1,23] / (df3[1,3] + (1 - (1 - (df3[1,12] / df3[1,13]))^2) * df3[1,13]*0.4)
   drtg <- team_defensive_rating + 0.2 * (100 * d_pts_per_scposs * (1 - stop) - team_defensive_rating)
   stats <- cbind(stats,round(stops/stats[2],2),round(stop,3),round(drtg,2))
   names(stats) =  c("Name","G","Stops","Stop%","DRtg")
   stats[is.na(stats)] <- 0
  return(stats)
}
