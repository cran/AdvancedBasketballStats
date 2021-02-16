#' @title Statistics by position
#' @description The function allows you to search for statistics by position within the lineup.
#' @param df1 Should be a Data Frame. The parameter has to be in the format provided by the lineups_data_adjustment() function.
#' @param n Should be a number. It represents the position on which you want to perform the search.
#' @details
#'                 The function allows you to search for paint players both in basic statistics and in extended statistics.
#'                 The supported values for n are as follows:
#' \itemize{
#'                 \item If the value entered for n is 1, it will return the statistics of the grouped Point Guards.
#'                 \item If the value entered for n is 2, it will return the statistics of the grouped Small Guards.
#'                 \item If the value entered for n is 3, it will return the statistics of the grouped Small Forwards.
#'                 \item If the value entered for n is 4, it will return the statistics of the grouped Paint Forwards.
#'                 \item If the value entered for n is 5, it will return the statistics of the grouped Centers.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the statistics by position.
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
#'  n <- 1
#'
#' lineups_players(df1,n)
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
#' n <- 5
#'
#' lineups_players(df1,n)
#'
#' @export
#'
#' @importFrom stats aggregate
#'

lineups_players <- function(df1,n){
  if(ncol(df1)==29){
    if(n==1){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M,df1$PM),
                       by=list(PG=df1$PG), FUN=sum)
      PFG<-sample(c(round(df1[3]/df1[4],3)), size = 1, replace = TRUE)
      P3P<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
      P2P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
      PFT<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
      df1 = cbind(df1,PFG,P3P,P2P,PFT)
      df1 = subset (df1, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
      df1[5][is.na(df1[5])] <- 0
      df1[8][is.na(df1[8])] <- 0
      df1[11][is.na(df1[11])] <- 0
      df1[14][is.na(df1[14])] <- 0
      names(df1) = c("PG","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
    }else if(n==2){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M,df1$PM),
                       by=list(SG=df1$SG), FUN=sum)
      PFG<-sample(c(round(df1[3]/df1[4],3)), size = 1, replace = TRUE)
      P3P<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
      P2P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
      PFT<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
      df1 = cbind(df1,PFG,P3P,P2P,PFT)
      df1 = subset (df1, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
      df1[5][is.na(df1[5])] <- 0
      df1[8][is.na(df1[8])] <- 0
      df1[11][is.na(df1[11])] <- 0
      df1[14][is.na(df1[14])] <- 0
      names(df1) = c("SG","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
    }else if(n==3){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M,df1$PM),
                       by=list(SF=df1$SF), FUN=sum)
      PFG<-sample(c(round(df1[3]/df1[4],3)), size = 1, replace = TRUE)
      P3P<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
      P2P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
      PFT<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
      df1 = cbind(df1,PFG,P3P,P2P,PFT)
      df1 = subset (df1, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
      df1[5][is.na(df1[5])] <- 0
      df1[8][is.na(df1[8])] <- 0
      df1[11][is.na(df1[11])] <- 0
      df1[14][is.na(df1[14])] <- 0
      names(df1) = c("SF","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
    }else if(n==4){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M,df1$PM),
                       by=list(PF=df1$PF), FUN=sum)
      PFG<-sample(c(round(df1[3]/df1[4],3)), size = 1, replace = TRUE)
      P3P<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
      P2P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
      PFT<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
      df1 = cbind(df1,PFG,P3P,P2P,PFT)
      df1 = subset (df1, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
      df1[5][is.na(df1[5])] <- 0
      df1[8][is.na(df1[8])] <- 0
      df1[11][is.na(df1[11])] <- 0
      df1[14][is.na(df1[14])] <- 0
      names(df1) = c("PF","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
    }else if(n==5){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","FGA","Percentage FG","TP","TPA","Percentage 3P","TWP","TWPA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$FGA,df1$TP,df1$TPA,df1$TWP,df1$TWPA,df1$FT,df1$FTA,df1$ORB,df1$DRB,df1$TRB,df1$AST,df1$STL,df1$BLK,df1$TOV,df1$PPF,df1$P,df1$M,df1$PM),
                       by=list(C=df1$C), FUN=sum)
      PFG<-sample(c(round(df1[3]/df1[4],3)), size = 1, replace = TRUE)
      P3P<-sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
      P2P<-sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
      PFT<-sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
      df1 = cbind(df1,PFG,P3P,P2P,PFT)
      df1 = subset (df1, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
      df1[5][is.na(df1[5])] <- 0
      df1[8][is.na(df1[8])] <- 0
      df1[11][is.na(df1[11])] <- 0
      df1[14][is.na(df1[14])] <- 0
      names(df1) = c("C","MP","FG","FGA","Percentage FG","3P","3PA","Percentage 3P","2P","2PA","Percentage 2P","FT","FTA","Percentage FT",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
    }
  }
  else if(ncol(df1)==41){
    if(n==1){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                             df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M,df1$PM),
                       by=list(PG=df1$PG), FUN=sum)
      names(df1) = c("PG","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
    }else if(n==2){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                             df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M,df1$PM),
                       by=list(SG=df1$SG), FUN=sum)
      names(df1) = c("SG","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
    }else if(n==3){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                             df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M,df1$PM),
                       by=list(SF=df1$SF), FUN=sum)
      names(df1) = c("SF","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
    }else if(n==4){
      names(df1) = c("PG","SG","SF","PPF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                             df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M,df1$PM),
                       by=list(PF=df1$PF), FUN=sum)
      names(df1) = c("PF","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
    }else if(n==5){
      names(df1) = c("PG","SG","SF","PF","C","MP","FG","oFG","FGA","oFGA","TP","oTP","TPA","oTPA","TWP","oTWP","TWPA","oTWPA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PPF","oPF","P","M","PM")
      df1 <- aggregate(cbind(df1$MP,df1$FG,df1$oFG,df1$FGA,df1$oFGA,df1$TP,df1$oTP,df1$TPA,df1$oTPA,df1$TWP,df1$oTWP,df1$TWPA,df1$oTWPA,df1$FT,df1$oFT,df1$FTA,df1$oFTA,df1$ORB,df1$oORB,
                             df1$DRB,df1$oDRB,df1$TRB,df1$oTRB,df1$AST,df1$oAST,df1$STL,df1$oSTL,df1$BLK,df1$oBLK,df1$TOV,df1$oTOV,df1$PPF,df1$oPF,df1$P,df1$M,df1$PM),
                       by=list(C=df1$C), FUN=sum)
      names(df1) = c("C","MP","FG","oFG","FGA","oFGA","3P","o3P","3PA","o3PA","2P","o2P","2PA","o2PA","FT","oFT","FTA","oFTA",
                     "ORB","oORB","DRB","oDRB","TRB","oTRB","AST","oAST","STL","oSTL","BLK","oBLK","TOV","oTOV","PF","oPF","+","-","+/-")
    }
  }
  return(df1)
}
