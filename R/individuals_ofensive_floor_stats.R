#' @title individual's offensive floor stats
#' @description The function allows the calculation of individual's offensive statistics on court
#' @param df1 Should be a Data Frame that represents the individual statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the team's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @param df3 Should be a Data Frame that represents the rival's statistics. The parameter has to be in the format provided by the team_stats() function.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the following individual's offensive statistics
#'        \itemize{
#'                 \item Score possessions (Sc. Poss)
#'                 \item Possessions attacked (Poss)
#'                 \item floor percentage (Floor\%)
#'                 \item Offensive Rating (ORtg)
#'                 \item Point produced per game (Pts Prod/G)
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
#'
#' individuals_ofensive_floor_stats(df1,df2,df3)
#'
#' @export
#'

individuals_ofensive_floor_stats <- function(df1,df2,df3){
  df1 <-   df1[-nrow(df1),]
  stats <- cbind(df1[1],df1[2])
   qast <- ((df1[4] / (df2[1,2] / 5)) * (1.14 * ((df2[1,18] - df1[20]) / df2[1,3]))) + ((((df2[1,18] / df2[1,2]) * df1[4] * 5 - df1[20]) / ((df2[1,3] / df2[1,2]) * df1[4] * 5 - df1[5])) * (1 - (df1[4] / (df2[1,2] / 5))))
   fg_part <- df1[5] * (1 - 0.5 * ((df1[25] - df1[14]) / (2 * df1[6])) * qast)
   fg_part[1][is.na(fg_part[1])] <- 0
   ast_part <- 0.5 * (((df2[1,23] - df2[1,12]) - (df1[25] - df1[14])) / (2 * (df2[1,4] - df1[6]))) * df1[20]
   ft_part <- (1-(1-(df1[14] / df1[15]))^2) * 0.4 * df1[15]
   ft_part[1][is.na(ft_part[1])] <- 0
   team_scoring_poss <- df2[1,3] + (1 - (1 - (df2[1,12] / df2[1,13]))^2) * df2[1,13] * 0.4
   team_orb <- df2[1,15] / (df2[1,15] + (df3[1,17] - df3[1,15]))
   team_play <- team_scoring_poss / (df2[1,4] + df2[1,13] * 0.4 + df2[1,21])
   team_orb_weight <- ((1 - team_orb) * team_play) / ((1 - team_orb) * team_play + team_orb * (1 - team_play))
   orb_part <- df1[17] * team_orb_weight * team_play
   scposs <- (fg_part + ast_part + ft_part) * (1 - (df2[1,15] / team_scoring_poss) * team_orb_weight * team_play) + orb_part
   fgxposs <- (df1[6] - df1[5]) * (1 - 1.07 * team_orb)
   ftxposs <- ((1 - (df1[14] / df1[15]))^2) * 0.4 * df1[15]
   ftxposs[1][is.na(ftxposs[1])] <- 0
   totposs <- scposs + fgxposs + ftxposs + df1[23]
   pprod_fg_part <- 2 * (df1[5] + 0.5 * df1[8]) * (1 - 0.5 * ((df1[25] - df1[14]) / (2 * df1[6])) * qast)
   pprod_fg_part[1][is.na(pprod_fg_part[1])] <- 0
   pprod_ast_part <- 2 * ((df2[1,3] - df1[5] + 0.5 * (df2[1,6] - df1[8])) / (df2[1,3] - df1[5])) * 0.5 * (((df2[1,23] - df2[1,12]) - (df1[25] - df1[14])) / (2 * (df2[1,4] - df1[6]))) * df1[20]
   pprod_orb_part <- df1[17] * team_orb_weight * team_play * (df2[1,23] / (df2[1,3] + (1 - (1 - (df2[1,12] / df2[1,13]))^2) * 0.4 * df2[1,13]))
   pprod = (pprod_fg_part + pprod_ast_part + df1[14]) * (1 - (df2[1,15] / team_scoring_poss) * team_orb_weight * team_play) + pprod_orb_part
   ortg <- 100 * (pprod / totposs)
   floor <- scposs / totposs
   stats <- cbind(stats,round(scposs/stats[2],2),round(totposs/stats[2],2),round(floor,3),round(ortg,2),round(pprod/stats[2],2))
   names(stats) =  c("Name","G","Sc. Poss","Poss","Floor%","ORtg","Pts Prod/G")
   stats[is.na(stats)] <- 0
  return(stats)
}
