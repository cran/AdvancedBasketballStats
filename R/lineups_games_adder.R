#' @title Lineups games adder
#' @description The function allows to perform the sums of two data.frames with the same format adopted after being transformed by lineups_data_adjustment() function,
#' @param df1 Should be a Data Frame that represents the first set of basic or extener lineups statistics. It has to be in the format provided by the lineups_data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the second set of basic or extener lineups statistics. It has to be in the format provided by the lineups_data_adjustment() function.
#' @details \itemize{
#'                 \item The function will work correctly when the name of the players is the same, in case it is different it will take the lineups as different.
#'                 \item The function sums data sets that have an identical size,
#'         }
#' @return Data frame with the sum of the statistics of the other two entered data.frame.
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
#' df2 <- data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
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
#' lineups_games_adder(df1,df2)
#'
#' @export
#'
#'
#' @importFrom stats aggregate
#'

lineups_games_adder <- function(df1,df2){
  if(ncol(df1) == 29 & ncol(df2) == 29){
    adder <- rbind(df1,df2)
    names(adder) <- c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                      "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
    adder <- aggregate(cbind(adder$MP,adder$FG,adder$FGA,adder$TP,adder$TPA,adder$TWP,adder$TWPA,adder$FT,adder$FTA,adder$ORB,
                             adder$DRB,adder$TRB,adder$AST,adder$STL,adder$BLK,adder$TOV,adder$PPF,adder$P,adder$M,adder$PM),
                       by=list(PG=adder$PG,SG=adder$SG,SF=adder$SF,PF=adder$PF,C=adder$C), FUN=sum)
    PFG <- sample(c(round(adder[7]/adder[8],3)), size = 1, replace = TRUE)
    P3P <- sample(c(round(adder[9]/adder[10],3)), size = 1, replace = TRUE)
    P2P <- sample(c(round(adder[11]/adder[12],3)), size = 1, replace = TRUE)
    PFT <- sample(c(round(adder[13]/adder[14],3)), size = 1, replace = TRUE)
    adder <- cbind(adder, PFG,P3P,P2P,PFT)
    adder <- subset (adder, select=c(1,2,3,4,5,6,7,8,26,9,10,27,11,12,28,13,14,29,15,16,17,18,19,20,21,22,23,24,25))
    adder[is.na(adder)] <- 0
    names(adder) <- c("PG","SG","SF","PF","C","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                      "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }
  else if (ncol(df1) == 41 & ncol(df2) == 41){
    adder <- rbind(df1,df2)
    names(adder) <- c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                      "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M","PM")
    adder <- aggregate(cbind(adder$MP,adder$FG,adder$oFG,adder$FGA,adder$oFGA,adder$TP,adder$oTP,adder$TPA,adder$oTPA,adder$TWP,adder$oTWP,adder$TWPA,adder$oTWPA,adder$FT,adder$oFT,adder$FTA,adder$oFTA,adder$ORB,adder$oORB,
                             adder$DRB,adder$oDRB,adder$TRB,adder$oTRB,adder$AST,adder$oAST,adder$STL,adder$oSTL,adder$BLK,adder$oBLK,adder$TOV,adder$oTOV,adder$PPF,adder$oPF,adder$P,adder$M,adder$PM),
                       by=list(PG=adder$PG,SG=adder$SG,SF=adder$SF,PF=adder$PF,C=adder$C), FUN=sum)
    names(adder) <- c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                      "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
  }
  adder[is.na(adder)] <- 0
  return(adder)
}
