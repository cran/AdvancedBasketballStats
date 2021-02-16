#' @title Lineups statistics comparator
#' @description The function allows the comparison of a lineup when it is in the court with the statistics of the rival
#' @param df1 Should be a Data Frame. The parameter has to be in the format provided by the lineups_data_adjustment() function.
#' @param m should be a number. This parameter has to be the duration of a single game.
#' @details The function only works with the extended statistics of the lineups.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the comparison of statistics and the following values:
#'        \itemize{
#'                 \item Lineup usage percentage (Team\%)
#'                 \item Pace (Pace)
#'                 \item Three rating (3Par)
#'                 \item True shooting percentage (TS\%)
#'                 \item Efficiency Field Goals percentage (eFG\%)
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
#' lineups_comparator_stats(df1,m)
#'
#' @export
#'

lineups_comparator_stats <- function(df1,m){
  if(ncol(df1)==41){
  adv_stats <-df1[1:6]
  tm_poss  <- df1[9] - (df1[23]/(df1[23] + df1[26])) * (df1[9] - df1[7]) * 1.07 + df1[35] + 0.4 * df1[21]
  opp_poss <- df1[10] - (df1[24]/(df1[24] + df1[25])) * (df1[10] - df1[8]) * 1.07 + df1[36] + 0.4 * df1[22]
  team <- df1[6]/cumsum(df1[6])
  pace <- m * ((tm_poss + opp_poss) / (2 * df1[6]))
  fg <- df1[7] - df1[8]
  fga <-df1[9] - df1[10]
  fg1 <- (df1[7]/df1[9])
  fg2 <- (df1[8]/df1[10])
  fg1[is.na(fg1)] <- 0; fg2[is.na(fg2)] <- 0
  fgp <- fg1 - fg2
  fgp[is.na(fgp)] <- 0
  tp <- df1[11] - df1[12]
  tpa <-df1[13] - df1[14]
  t1 <- (df1[11]/df1[13])
  t2 <- (df1[12]/df1[14])
  t1[is.na(t1)] <- 0; t2[is.na(t2)] <- 0
  tgp <- t1 - t2
  twp <- df1[15] - df1[16]
  twpa <-df1[17] - df1[18]
  tw1 <- (df1[15]/df1[17])
  tw2 <- (df1[16]/df1[18])
  tw1[is.na(tw1)] <- 0; tw2[is.na(tw2)] <- 0
  twgp <- tw1 - tw2
  efg <- (df1[7] + 0.5 * df1[11]) / df1[9]
  efg_opp <- (df1[8] + 0.5 * df1[12]) / df1[10]
  efg[is.na(efg)] <- 0; efg_opp[is.na(efg_opp)] <- 0
  efgp<- efg - efg_opp
  ts <- df1[39] / (2 * (df1[9] + 0.44 * df1[21]))
  ts_opp <- df1[40] / (2 * (df1[10] + 0.44 * df1[22]))
  ts[is.na(ts)] <- 0; ts_opp[is.na(ts_opp)] <- 0
  tsp <- ts - ts_opp
  ft <- df1[19] - df1[20]
  fta <-df1[21] - df1[22]
  f1 <-(df1[19]/df1[21])
  f2 <- (df1[20]/df1[22])
  f1[is.na(f1)] <- 0; f2[is.na(f2)] <- 0
  ftp <- f1 - f2
  pm <- df1[41]
  adv_stats <- cbind(adv_stats,round(team,3),round(pace,2),fg,fga,round(fgp,3),tp,tpa,round(tgp,3),twp,twpa,round(twgp,3),round(efgp,3),round(tsp,3),ft,fta,round(ftp,3),pm)
  names(adv_stats) =  c("PG","SG","SF","PF","C","MP","Team%","Pace","FG","FGA","FG%","3P","3PA","3P%",
                        "2P","2PA","2P%","eFG%","TS%","FT","FTA","FT%","+/-")
  }else if (ncol(df1)==39){
    adv_stats <-df1[1:4]
    tm_poss  <- df1[7] - (df1[21]/(df1[21] + df1[24])) * (df1[7] - df1[5]) * 1.07 + df1[33] + 0.4 * df1[19]
    opp_poss <- df1[8] - (df1[22]/(df1[22] + df1[23])) * (df1[8] - df1[6]) * 1.07 + df1[34] + 0.4 * df1[20]
    team <- df1[4]/cumsum(df1[4])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[4]))
    fg <- df1[5] - df1[6]
    fga <-df1[7] - df1[8]
    fg1 <- (df1[5]/df1[7])
    fg2 <- (df1[6]/df1[8])
    fg1[is.na(fg1)] <- 0; fg2[is.na(fg2)] <- 0
    fgp <- fg1 - fg2
    fgp[is.na(fgp)] <- 0
    tp <- df1[9] - df1[10]
    tpa <-df1[11] - df1[12]
    t1 <- (df1[9]/df1[11])
    t2 <- (df1[10]/df1[12])
    t1[is.na(t1)] <- 0; t2[is.na(t2)] <- 0
    tgp <- t1 - t2
    twp <- df1[13] - df1[14]
    twpa <-df1[15] - df1[16]
    tw1 <- (df1[13]/df1[15])
    tw2 <- (df1[14]/df1[16])
    tw1[is.na(tw1)] <- 0; tw2[is.na(tw2)] <- 0
    twgp <- tw1 - tw2
    efg <- (df1[5] + 0.5 * df1[9]) / df1[7]
    efg_opp <- (df1[6] + 0.5 * df1[10]) / df1[8]
    efg[is.na(efg)] <- 0; efg_opp[is.na(efg_opp)] <- 0
    efgp<- efg - efg_opp
    ts <- df1[37] / (2 * (df1[7] + 0.44 * df1[19]))
    ts_opp <- df1[38] / (2 * (df1[8] + 0.44 * df1[20]))
    ts[is.na(ts)] <- 0; ts_opp[is.na(ts_opp)] <- 0
    tsp <- ts - ts_opp
    ft <- df1[17] - df1[18]
    fta <-df1[19] - df1[20]
    f1 <-(df1[17]/df1[19])
    f2 <- (df1[18]/df1[20])
    f1[is.na(f1)] <- 0; f2[is.na(f2)] <- 0
    ftp <- f1 - f2
    pm <- df1[39]
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,2),fg,fga,round(fgp,3),tp,tpa,round(tgp,3),twp,twpa,round(twgp,3),round(efgp,3),round(tsp,3),ft,fta,round(ftp,3),pm)
    names(adv_stats) =  c("PG","SG","SF","MP","Team%","Pace","FG","FGA","FG%","3P","3PA","3P%",
                          "2P","2PA","2P%","eFG%","TS%","FT","FTA","FT%","+/-")
  }else if (ncol(df1)==38){
    adv_stats <-df1[1:3]
    tm_poss  <- df1[6] - (df1[20]/(df1[20] + df1[23])) * (df1[6] - df1[4]) * 1.07 + df1[32] + 0.4 * df1[18]
    opp_poss <- df1[7] - (df1[21]/(df1[21] + df1[22])) * (df1[7] - df1[5]) * 1.07 + df1[33] + 0.4 * df1[19]
    team <- df1[3]/cumsum(df1[3])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[3]))
    fg <- df1[4] - df1[5]
    fga <-df1[6] - df1[7]
    fg1 <- (df1[4]/df1[6])
    fg2 <- (df1[5]/df1[7])
    fg1[is.na(fg1)] <- 0; fg2[is.na(fg2)] <- 0
    fgp <- fg1 - fg2
    fgp[is.na(fgp)] <- 0
    tp <- df1[8] - df1[9]
    tpa <-df1[10] - df1[11]
    t1 <- (df1[8]/df1[10])
    t2 <- (df1[9]/df1[11])
    t1[is.na(t1)] <- 0; t2[is.na(t2)] <- 0
    tgp <- t1 - t2
    twp <- df1[12] - df1[13]
    twpa <-df1[14] - df1[15]
    tw1 <- (df1[12]/df1[14])
    tw2 <- (df1[13]/df1[15])
    tw1[is.na(tw1)] <- 0; tw2[is.na(tw2)] <- 0
    twgp <- tw1 - tw2
    efg <- (df1[4] + 0.5 * df1[8]) / df1[6]
    efg_opp <- (df1[5] + 0.5 * df1[9]) / df1[7]
    efg[is.na(efg)] <- 0; efg_opp[is.na(efg_opp)] <- 0
    efgp<- efg - efg_opp
    ts <- df1[36] / (2 * (df1[6] + 0.44 * df1[18]))
    ts_opp <- df1[37] / (2 * (df1[7] + 0.44 * df1[19]))
    ts[is.na(ts)] <- 0; ts_opp[is.na(ts_opp)] <- 0
    tsp <- ts - ts_opp
    ft <- df1[16] - df1[17]
    fta <-df1[18] - df1[19]
    f1 <-(df1[16]/df1[18])
    f2 <- (df1[17]/df1[19])
    f1[is.na(f1)] <- 0; f2[is.na(f2)] <- 0
    ftp <- f1 - f2
    pm <- df1[38]
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,2),fg,fga,round(fgp,3),tp,tpa,round(tgp,3),twp,twpa,round(twgp,3),round(efgp,3),round(tsp,3),ft,fta,round(ftp,3),pm)
    names(adv_stats) =  c("PF","C","MP","Team%","Pace","FG","FGA","FG%","3P","3PA","3P%",
                          "2P","2PA","2P%","eFG%","TS%","FT","FTA","FT%","+/-")
  }else if (ncol(df1)==37){
    adv_stats <-df1[1:2]
    tm_poss  <- df1[5] - (df1[19]/(df1[19] + df1[22])) * (df1[5] - df1[3]) * 1.07 + df1[31] + 0.4 * df1[17]
    opp_poss <- df1[6] - (df1[20]/(df1[20] + df1[21])) * (df1[6] - df1[4]) * 1.07 + df1[32] + 0.4 * df1[18]
    team <- df1[2]/cumsum(df1[2])
    pace <- m * ((tm_poss + opp_poss) / (2 * df1[2]))
    fg <- df1[3] - df1[4]
    fga <-df1[5] - df1[6]
    fg1 <- (df1[3]/df1[5])
    fg2 <- (df1[4]/df1[6])
    fg1[is.na(fg1)] <- 0; fg2[is.na(fg2)] <- 0
    fgp <- fg1 - fg2
    fgp[is.na(fgp)] <- 0
    tp <- df1[7] - df1[8]
    tpa <-df1[9] - df1[10]
    t1 <- (df1[7]/df1[9])
    t2 <- (df1[8]/df1[10])
    t1[is.na(t1)] <- 0; t2[is.na(t2)] <- 0
    tgp <- t1 - t2
    twp <- df1[11] - df1[12]
    twpa <-df1[13] - df1[14]
    tw1 <- (df1[11]/df1[13])
    tw2 <- (df1[12]/df1[14])
    tw1[is.na(tw1)] <- 0; tw2[is.na(tw2)] <- 0
    twgp <- tw1 - tw2
    efg <- (df1[3] + 0.5 * df1[7]) / df1[5]
    efg_opp <- (df1[4] + 0.5 * df1[8]) / df1[6]
    efg[is.na(efg)] <- 0; efg_opp[is.na(efg_opp)] <- 0
    efgp<- efg - efg_opp
    ts <- df1[35] / (2 * (df1[5] + 0.44 * df1[17]))
    ts_opp <- df1[36] / (2 * (df1[6] + 0.44 * df1[18]))
    ts[is.na(ts)] <- 0; ts_opp[is.na(ts_opp)] <- 0
    tsp <- ts - ts_opp
    ft <- df1[15] - df1[16]
    fta <-df1[17] - df1[18]
    f1 <-(df1[15]/df1[17])
    f2 <- (df1[16]/df1[18])
    f1[is.na(f1)] <- 0; f2[is.na(f2)] <- 0
    ftp <- f1 - f2
    pm <- df1[37]
    adv_stats <- cbind(adv_stats,round(team,3),round(pace,2),fg,fga,round(fgp,3),tp,tpa,round(tgp,3),twp,twpa,round(twgp,3),round(efgp,3),round(tsp,3),ft,fta,round(ftp,3),pm)
    names(adv_stats) =  c("Name","MP","Team%","Pace","FG","FGA","FG%","3P","3PA","3P%",
                          "2P","2PA","2P%","eFG%","TS%","FT","FTA","FT%","+/-")
  }
  adv_stats[is.na(adv_stats)] <- 0
  return(adv_stats)
}

