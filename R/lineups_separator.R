#' @title Statistics separator
#' @description The function allows to separate the extended statistics of the lineups. Therefore, you can obtain the statistics of the lineups or the rival with respect to the lineups for later analysis
#' @param df1 Should be a Data Frame. The parameter has to be in the format provided by the lineups_data_adjustment() function.
#' @param n Should be a number. it Represents the statistics we want to obtain.
#' @details The function only works with the extended statistics of the lineups.
#'          The supported values for n are as follows:
#' \itemize{
#'                 \item If n takes the value of 1, the function will return the statistics of the lineup.
#'                 \item If n takes the value of 2, the function will return the statistics of the rival with respect to the lineup.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henare
#' @return Data frame with the statistics separated in the format of the basic statistics of the lineups.
#' @examples
#'
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
#' n <- 1
#'
#' lineups_separator(df1,n)
#'
#' n <- 2
#'
#' lineups_separator(df1,n)
#'
#'
#' @export

lineups_separator <- function(df1,n){
  if(ncol(df1)==41){
    separator <- df1[1:6]
    for(i in 7:38){
      if(n==1 & (i%%2!=0)){
        separator<- cbind(separator,df1[i])
      }
      else if (n==2 & (i%%2==0)){
        separator<- cbind(separator,df1[i])
      }
    }
    separator<- cbind(separator,df1[39:41])
    PFG<-sample(c(round(separator[7]/separator[8],3)), size = 1, replace = TRUE)
    P3P<-sample(c(round(separator[9]/separator[10],3)), size = 1, replace = TRUE)
    P2P<-sample(c(round(separator[11]/separator[12],3)), size = 1, replace = TRUE)
    PFT<-sample(c(round(separator[13]/separator[14],3)), size = 1, replace = TRUE)
    separator <- cbind(separator, PFG,P3P,P2P,PFT)
    separator <- subset (separator, select=c(1,2,3,4,5,6,7,8,26,9,10,27,11,12,28,13,14,29,15,16,17,18,19,20,21,22,23,24,25))
    separator[9][is.na(separator[9])] <- 0
    separator[12][is.na(separator[12])] <- 0
    separator[15][is.na(separator[15])] <- 0
    separator[18][is.na(separator[18])] <- 0
    names(separator) = c("PG","SG","SF","PF","C","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }
  else if(ncol(df1)==39){
    separator <- df1[1:4]
    for(i in 5:36){
      if(n==1 & (i%%2!=0)){
        separator<- cbind(separator,df1[i])
      }
      else if (n==2 & (i%%2==0)){
        separator<- cbind(separator,df1[i])
      }
    }
    separator<- cbind(separator,df1[37:39])
    PFG<-sample(c(round(separator[5]/separator[6],3)), size = 1, replace = TRUE)
    P3P<-sample(c(round(separator[7]/separator[8],3)), size = 1, replace = TRUE)
    P2P<-sample(c(round(separator[9]/separator[10],3)), size = 1, replace = TRUE)
    PFT<-sample(c(round(separator[11]/separator[12],3)), size = 1, replace = TRUE)
    separator <- cbind(separator, PFG,P3P,P2P,PFT)
    separator <- subset (separator, select=c(1,2,3,4,5,6,24,7,8,25,9,10,26,11,12,27,13,14,15,16,17,18,19,20,21,22,23))
    separator[7][is.na(separator[7])] <- 0
    separator[10][is.na(separator[10])] <- 0
    separator[13][is.na(separator[13])] <- 0
    separator[16][is.na(separator[16])] <- 0
    names(separator) = c("PG","SG","SF","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                         "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }
  else if(ncol(df1)==38){
    separator <- df1[1:3]
    for(i in 4:35){
      if(n==1 & (i%%2==0)){
        separator<- cbind(separator,df1[i])
      }
      else if (n==2 & (i%%2!=0)){
        separator<- cbind(separator,df1[i])
      }
    }
    separator<- cbind(separator,df1[36:38])
    PFG<-sample(c(round(separator[4]/separator[5],3)), size = 1, replace = TRUE)
    P3P<-sample(c(round(separator[6]/separator[7],3)), size = 1, replace = TRUE)
    P2P<-sample(c(round(separator[8]/separator[9],3)), size = 1, replace = TRUE)
    PFT<-sample(c(round(separator[10]/separator[11],3)), size = 1, replace = TRUE)
    separator <- cbind(separator, PFG,P3P,P2P,PFT)
    separator <- subset (separator, select=c(1,2,3,4,5,23,6,7,24,8,9,25,10,11,26,12,13,14,15,16,17,18,19,20,21,22))
    separator[6][is.na(separator[6])] <- 0
    separator[9][is.na(separator[9])] <- 0
    separator[12][is.na(separator[12])] <- 0
    separator[15][is.na(separator[15])] <- 0
    names(separator) = c("PF","C","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                         "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }else if(ncol(df1)==37){
    separator <- df1[1:2]
    for(i in 3:34){
      if(n==1 & (i%%2!=0)){
        separator<- cbind(separator,df1[i])
      }
      else if (n==2 & (i%%2==0)){
        separator<- cbind(separator,df1[i])
      }
    }
    separator<- cbind(separator,df1[35:37])
    PFG<-sample(c(round(separator[3]/separator[4],3)), size = 1, replace = TRUE)
    P3P<-sample(c(round(separator[5]/separator[6],3)), size = 1, replace = TRUE)
    P2P<-sample(c(round(separator[7]/separator[8],3)), size = 1, replace = TRUE)
    PFT<-sample(c(round(separator[9]/separator[10],3)), size = 1, replace = TRUE)
    separator <- cbind(separator, PFG,P3P,P2P,PFT)
    separator <- subset (separator, select=c(1,2,3,4,22,5,6,23,7,8,24,9,10,25,11,12,13,14,15,16,17,18,19,20,21))
    separator[5][is.na(separator[5])] <- 0
    separator[8][is.na(separator[8])] <- 0
    separator[11][is.na(separator[11])] <- 0
    separator[14][is.na(separator[14])] <- 0
    names(separator) = c("Name","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                         "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","+","-","+/-")
  }
  return(separator)
}

