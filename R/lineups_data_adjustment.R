#' @title Lineups data adjustment
#' @description The function transform the statistics entered for later use in the rest of the functions that apply to lineup statistics.
#' @param df1 Should be a Data Frame
#' @details \itemize{
#'                 \item The data.frame must have the same columns and these represent the same as in the example.
#'                 \item The function allows the transformation of the basic statistics of the lineups to which the shooting percentages, the total rebounds and the plus minus.
#'                 \item The function allows the transformation of the extended statistics of the lineups to which the total rebounds and the plus minus.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return The data frame obtained for the basic statistics of lineups will have the following format:
#'         \itemize{
#'                 \item Point Guard (PG)
#'                 \item Shooting Guard (SG)
#'                 \item Small Forward (SF)
#'                 \item Paint Forward (PF)
#'                 \item Center (C)
#'                 \item Games played (G)
#'                 \item Games Started (GS)
#'                 \item Minutes Played (MP)
#'                 \item Field Goals Made (FG)
#'                 \item Field Goals Attempted (FGA)
#'                 \item Field Goals Percentage (FG%)
#'                 \item Three Points Made (3P)
#'                 \item Three Points Attempted (3PA)
#'                 \item Three Points Percentage (3P\%)
#'                 \item Two Points Made (2P)
#'                 \item Two Points Attempted (2PA)
#'                 \item Free Throw Made (FT)
#'                 \item Offensive Rebounds (ORB)
#'                 \item Defensive Rebounds (DRB)
#'                 \item Total Rebounds (TRB)
#'                 \item Assists (AST)
#'                 \item Steals (STL)
#'                 \item Blocks (BLK)
#'                 \item Turnover (TOV)
#'                 \item Personal Fouls (PF)
#'                 \item Points (PTS)
#'                 \item Plus (+)
#'                 \item Minus (-)
#'                 \item Plus Minus (+/-)
#'         }
#' For the extended statistics of the lineups it will have the same format as the basic statistics of the lineups but adding the statistics of the opponent against that lineups.
#'
#'
#' @examples
#'
#' df1 <- data.frame("PG"= c("James","Rondo"),"SG" = c("Green","Caruso"),
#' "SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
#' "C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(4,0),
#' "FGA"  = c(7,0), "X3P" = c(0,0),"X3PA" = c(2,0),"X2P" = c(4,0),
#' "X2PA" = c(5,0),  "FT" = c(1,0), "FTA" = c(3,0),"ORB" = c(2,0),
#' "DRB" = c(5,0), "AST " = c(2,0), "STL " = c(1,0), "BLK " = c(0,0),
#' "TOV " = c(7,2), "PF" = c(1,0),  "PLUS" = c(9,0),"MINUS" = c(17,3))
#'
#' lineups_data_adjustment(df1)
#'
#' df1 <-  data.frame("PG" = c("James","Rondo"),"SG"= c("Green","Caruso"),
#' "SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
#' "C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(6,0),
#' "OppFG " = c(6,0), "FGA " = c(10,0),"OppFGA " = c(9,0),
#' "X3P  " = c(2,0),"Opp3P  " = c(1,0),"X3PA " = c(4,0),
#' "Opp3PA  ç" = c(3,0),"X2P" = c(4,0),"Opp2P" = c(5,0),"X2PA " = c(6,0),
#' "Opp2PA" = c(8,0) , "FT " = c(0,0),"OppFT " = c(1,0), "FTA " = c(0,0),
#' "OppFTA" = c(1,0),"OppRB" = c(2,0),"OppOppRB" = c(1,0),"DRB" = c(4,0),
#' "OppDRB" = c(1,0),"AST " = c(5,0),"OppAST " = c(4,0),"STL" = c(1,0),
#' "OppSTL" = c(3,0),"BLK" = c(0,0),"OppBLK" = c(1,0),"TOppV" = c(5,2),
#' "OppTOppV" = c(3,2),"PF" = c(1,0),"OppPF" = c(3,0),"PLUS" = c(15,0),
#' "MINUS" = c(14,3))
#'
#'
#' lineups_data_adjustment(df1)
#'
#' @export
#'
#'
#' @importFrom stats aggregate
#'

lineups_data_adjustment <- function(df1){
  if(ncol(df1)==23){

    names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","TP","TPA","TWP","TWPA","FT","FTA",
                   "ORB","DRB","AST","STL","BLK","TOV","PPF","P","M")
    data_adjustment <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M),
                                 by=list(PG=df1$PG,SG=df1$SG,SF=df1$SF,PF=df1$PF,C=df1$C), FUN=sum)
    PFG <- sample(c(round(data_adjustment[7]/data_adjustment[8],3)), size = 1, replace = TRUE)
    P3P <- sample(c(round(data_adjustment[9]/data_adjustment[10],3)), size = 1, replace = TRUE)
    P2P <- sample(c(round(data_adjustment[11]/data_adjustment[12],3)), size = 1, replace = TRUE)
    PFT <- sample(c(round(data_adjustment[13]/data_adjustment[14],3)), size = 1, replace = TRUE)
    TOTR <- sample(c(round(data_adjustment[15]+data_adjustment[16],3)), size = 1, replace = TRUE)
    PM <- sample(c(round(data_adjustment[22]-data_adjustment[23],3)), size = 1, replace = TRUE)
    data_adjustment <- cbind(data_adjustment, PFG,P3P,P2P,PFT,TOTR,PM)
    data_adjustment <- subset (data_adjustment, select=c(1,2,3,4,5,6,7,8,24,9,10,25,11,12,26,13,14,27,15,16,28,17,18,19,20,21,22,23,29))
    data_adjustment[is.na(data_adjustment)] <- 0

    names(data_adjustment) = c("PG","SG","SF","PF","C","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                               "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")

  }
  else if(ncol(df1)==38){
    names(df1) <- c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                    "ORB","oORB","DRB","oDRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M")
    data_adjustment <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                                       df1$DRB,df1$oDRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M),
                                 by=list(PG=df1$PG,SG=df1$SG,SF=df1$SF,PF=df1$PF,C=df1$C), FUN=sum)
    TOTR <- sample(c(round(data_adjustment[23]+data_adjustment[25],3)), size = 1, replace = TRUE)
    oTOTR <- sample(c(round(data_adjustment[24]+data_adjustment[26],3)), size = 1, replace = TRUE)
    PM <- sample(c(round(data_adjustment[37]-data_adjustment[38],3)), size = 1, replace = TRUE)
    data_adjustment <- cbind(data_adjustment,TOTR,oTOTR,PM)
    data_adjustment <- subset (data_adjustment, select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,39,40,27,28,29,30,31,32,33,34,35,36,37,38,41))
    names(data_adjustment) <- c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                                "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")

  }
  data_adjustment[is.na(data_adjustment)] <- 0
  return(data_adjustment)
}
