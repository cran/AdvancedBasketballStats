#' @title Statistics searcher of backcourt players
#' @description The function allows find the statisticts of backcourt players
#' @param df1 Should be a Data Frame. The parameter has to be in the format provided by the lineups_data_adjustment() function.
#' @details The function works with the basic statistics of the lineups and the extended statistics of the lineups.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the statistics of the backcourt players
#' @examples
#'
#' df1 <- data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
#' "SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
#' "C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(4,0),
#' "FGA " = c(7,0),"Percentage FG" = c(0.571,0),
#' "X3P  " = c(0,0),"X3PA  " = c(2,0),"Percentage 3P" = c(0,0),
#' "X2P " = c(4,0), "X2PA " = c(5,0), "Percentage 2P" = c(0.8,0),
#' "FT " = c(1,0), "FTA " = c(3,0), "Percentage FT" = c(0.333,0),
#' "ORB " = c(2,0), "DRB " = c(5,0),"TRB " = c(7,0), "AST " = c(2,0),
#' "STL " = c(1,0), "BLK " = c(0,0),"TOV " = c(7,2), "PF" = c(1,0),
#' "PLUS" = c(9,0),"MINUS" = c(17,3),"P/M" = c(-8,-3))
#'
#' lineups_backcourt(df1)
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
#' lineups_backcourt(df1)
#'
#' @export
#'
#' @importFrom stats aggregate
#'

lineups_backcourt <- function(df1){
  if(ncol(df1)==29){
  names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                 "ORB","DRB","TRB","AST","STL","BLK","TOV","PFF","P","M","PM")
  df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PFF,df1$P,df1$M,df1$PM), by=list(PG=df1$PG,SG=df1$SG,SF=df1$SF), FUN=sum)
  PFG<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
  P3P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
  P2P<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
  PFT<-sample(c(round(df1[11]/df1[12],3)), size = 1, replace = TRUE)
  MM <- sample(c(round(df1[21]-df1[22],3)), size = 1, replace = TRUE)
  df1 = cbind(df1, PFG,P3P,P2P,PFT,MM)
  df1 = subset (df1, select=c(1,2,3,4,5,6,24,7,8,25,9,10,26,11,12,27,13,14,15,16,17,18,19,20,21,22,28))
  df1[7][is.na(df1[7])] <- 0
  df1[10][is.na(df1[10])] <- 0
  df1[13][is.na(df1[13])] <- 0
  df1[16][is.na(df1[16])] <- 0
  names(df1) = c("PG","SG","SF","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                 "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }
  else if(ncol(df1)==41){
    names(df1) = c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                               "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PFF","oPF","P","M","PM")
    df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                                       df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PFF,df1$oPF,df1$P,df1$M,df1$PM),
                                 by=list(PG=df1$PG,SG=df1$SG,SF=df1$SF), FUN=sum)
    names(df1) = c("PG","SG","SF","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                               "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
  }
  return(df1)
}
