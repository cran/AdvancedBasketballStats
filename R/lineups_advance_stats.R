#' @title Lineups advanced statistics
#' @description This function allows the calculation of advanced player statistics.
#' @param df1 Should be a Data Frame. This parameter has to be in the format provided by the lineups_advance_stats() function.
#' @param m should be a number. This parameter has to be the duration of a single game.
#' @details The function only works with the extended statistics of the lineups.
#' @return Data frame with the following advanced statistics calculated
#'        \itemize{
#'                 \item Offensive Rating (ORtg)
#'                 \item Defensive Rating (DRtg)
#'                 \item Net Rating (NetRtg)
#'                 \item Pace (Pace)
#'                 \item Three rating (3Par)
#'                 \item True shooting percentage (TS\%)
#'                 \item Efficiency Field Goals percentage (eFG\%)
#'                 \item Assists percentage (AST\%)
#'                 \item Offensive rebounds percentage (ORB\%)
#'                 \item Defensive rebounds percentage (DRB\%)
#'                 \item Total rebounds percentage (TRB\%)
#'                 \item Turnover percentage (TOV\%)
#'         }
#' @examples
#'
#' df1 <-  data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
#' "SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
#' "C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(6,0),
#' "OppFG " = c(6,0), "FGA " = c(10,0),"OppFGA " = c(9,0),
#' "X3P  " = c(2,0),"Opp3P" = c(1,0),"X3PA" = c(4,0),"Opp3PA" = c(3,0),
#' "X2P" = c(4,0),"Opp2P " = c(5,0), "X2PA " = c(6,0),"Opp2PA " = c(8,0) ,
#' "FT " = c(0,0),"OppFT " = c(1,0), "FTA " = c(0,0),"OppFTA " = c(1,0),
#' "OppRB " = c(2,0),"OppOppRB " = c(1,0), "DRB" = c(4,0),"OppDRB" = c(1,0),
#' "TRB" = c(6,0),"OppTRB" = c(2,0), "AST " = c(5,0),"OppAST " = c(4,0),
#' "STL " = c(1,0),"OppSTL " = c(3,0), "BLK " = c(0,0),  "OppBLK " = c(1,0),
#' "TOppV " = c(5,2), "OppTOppV " = c(3,2),"PF" = c(1,0),"OppPF" = c(3,0),
#' "PLUS" = c(15,0),"MINUS" = c(14,3),"P/M" = c(1,-3))
#'
#' m <- 48
#'
#' lineups_advance_stats(df1,m)
#'
#' @export
#'

lineups_advance_stats <- function(df1,m){
  if(ncol(df1)==41){
    adv_stats <-df1[1:6]
    tm_poss  <- df1[9] - (df1[23]/(df1[23] + df1[26])) * (df1[9] - df1[7]) * 1.07 + df1[35] + 0.4 * df1[21]
    opp_poss <- df1[10] - (df1[24]/(df1[24] + df1[25])) * (df1[10] - df1[8]) * 1.07 + df1[36] + 0.4 * df1[22]
    team <- df1[6]/cumsum(df1[6])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[6]))
    offrtg <- 100*((df1[39])/(tm_poss))
    defrtg <- 100*((df1[40])/(opp_poss))
    netrtg <- offrtg - defrtg
    par <- df1[13]/df1[9]
    ftr <- df1[21]/df1[9]
    ts <- df1[39] / (2 * (df1[9] + 0.44 * df1[21]))
    efg <- (df1[7] + 0.5 * df1[11]) / df1[9]
    ast <- df1[29] / df1[7]
    orb <- (df1[23]/(df1[23] + df1[26]))
    drb <- df1[25] / (df1[24] + df1[25])
    trb <- df1[27] / (df1[27] + df1[28])
    tov <- df1[35] / (df1[9] + 0.44 * df1[21] + df1[35])
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,3),round(offrtg,2),round(defrtg,2),round(netrtg,2),round(par,3),
                                       round(ftr,3),round(ts,3),round(efg,3),round(ast,3),round(orb,3),round(drb,3),round(trb,3),round(tov,3))
    names(adv_stats) =  c("PG","SG","SF","PF","C","MP","TEAM%","PACE","ORtg","DRtg","Net Rtg","3Par","FTr","TS%","eFG%","AST%","ORB%","DRB%","TRB%","TOV%")
  }else if(ncol(df1)==39){
    adv_stats <-df1[1:4]
    tm_poss  <- df1[7] - (df1[21]/(df1[21] + df1[24])) * (df1[7] - df1[5]) * 1.07 + df1[33] + 0.4 * df1[19]
    opp_poss <- df1[8] - (df1[22]/(df1[22] + df1[23])) * (df1[8] - df1[6]) * 1.07 + df1[34] + 0.4 * df1[20]
    team <- df1[4]/cumsum(df1[4])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[4]))
    offrtg <- 100*((df1[37])/(tm_poss))
    defrtg <- 100*((df1[38])/(opp_poss))
    netrtg <- offrtg - defrtg
    par <- df1[11]/df1[7]
    ftr <- df1[19]/df1[7]
    ts <- df1[37] / (2 * (df1[7] + 0.44 * df1[19]))
    efg <- (df1[5] + 0.5 * df1[9]) / df1[7]
    ast <- df1[27] / df1[5]
    orb <- (df1[21]/(df1[21] + df1[24]))
    drb <- df1[23] / (df1[22] + df1[23])
    trb <- df1[25] / (df1[25] + df1[26])
    tov <- df1[33] / (df1[7] + 0.44 * df1[19] + df1[33])
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,3),round(offrtg,2),round(defrtg,2),round(netrtg,2),round(par,3),
                       round(ftr,3),round(ts,3),round(efg,3),round(ast,3),round(orb,3),round(drb,3),round(trb,3),round(tov,3))
    names(adv_stats) =  c("PG","SG","SF","MP","TEAM%","PACE","ORtg","DRtg","Net Rtg","3Par","FTr","TS%","eFG%","AST%","ORB%","DRB%","TRB%","TOV%")
  }else if(ncol(df1)==38){
    adv_stats <-df1[1:3]
    tm_poss  <- df1[6] - (df1[20]/(df1[20] + df1[23])) * (df1[6] - df1[4]) * 1.07 + df1[32] + 0.4 * df1[18]
    opp_poss <- df1[7] - (df1[21]/(df1[21] + df1[22])) * (df1[7] - df1[5]) * 1.07 + df1[33] + 0.4 * df1[19]
    team <- df1[3]/cumsum(df1[3])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[3]))
    offrtg <- 100*((df1[36])/(tm_poss))
    defrtg <- 100*((df1[37])/(opp_poss))
    netrtg <- offrtg - defrtg
    par <- df1[10]/df1[6]
    ftr <- df1[18]/df1[6]
    ts <- df1[36] / (2 * (df1[6] + 0.44 * df1[18]))
    efg <- (df1[4] + 0.5 * df1[8]) / df1[6]
    ast <- df1[26] / df1[4]
    orb <- (df1[20]/(df1[20] + df1[23]))
    drb <- df1[22] / (df1[21] + df1[22])
    trb <- df1[24] / (df1[24] + df1[25])
    tov <- df1[32] / (df1[6] + 0.44 * df1[18] + df1[32])
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,3),round(offrtg,2),round(defrtg,2),round(netrtg,2),round(par,3),
                       round(ftr,3),round(ts,3),round(efg,3),round(ast,3),round(orb,3),round(drb,3),round(trb,3),round(tov,3))
    names(adv_stats) =  c("PF","C","MP","TEAM%","PACE","ORtg","DRtg","Net Rtg","3Par","FTr","TS%","eFG%","AST%","ORB%","DRB%","TRB%","TOV%")
  }else if(ncol(df1)==37){
    adv_stats <-df1[1:2]
    tm_poss  <- df1[5] - (df1[19]/(df1[19] + df1[22])) * (df1[5] - df1[3]) * 1.07 + df1[31] + 0.4 * df1[17]
    opp_poss <- df1[6] - (df1[20]/(df1[20] + df1[21])) * (df1[6] - df1[4]) * 1.07 + df1[32] + 0.4 * df1[18]
    team <- df1[2]/cumsum(df1[2])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[2]))
    offrtg <- 100*((df1[35])/(tm_poss))
    defrtg <- 100*((df1[36])/(opp_poss))
    netrtg <- offrtg - defrtg
    par <- df1[9]/df1[5]
    ftr <- df1[17]/df1[5]
    ts <- df1[35] / (2 * (df1[5] + 0.44 * df1[17]))
    efg <- (df1[3] + 0.5 * df1[7]) / df1[5]
    ast <- df1[25] / df1[3]
    orb <- (df1[19]/(df1[19] + df1[22]))
    drb <- df1[21] / (df1[20] + df1[21])
    trb <- df1[23] / (df1[23] + df1[24])
    tov <- df1[31] / (df1[5] + 0.44 * df1[17] + df1[31])
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,3),round(offrtg,2),round(defrtg,2),round(netrtg,2),round(par,3),
                       round(ftr,3),round(ts,3),round(efg,3),round(ast,3),round(orb,3),round(drb,3),round(trb,3),round(tov,3))
    names(adv_stats) =  c("Name","MP","TEAM%","Pace","ORtg","DRtg","Net Rtg","3Par","FTr","TS%","eFG%","AST%","ORB%","DRB%","TRB%","TOV%")
  }
  adv_stats[is.na(adv_stats)] <- 0
  return(adv_stats)
}

