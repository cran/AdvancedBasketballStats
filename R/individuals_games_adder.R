#' @title individual's games adder
#' @description The function allows to perform the sums of two data.frames with the same format adopted after being transformed by individuals_data_adjustment() function,
#' @param df1 Should be a Data Frame that represents the first set of individual statistics or defensive individual statistics of the players. It has to be in the format provided by the individuals_data_adjustment() function
#' @param df2 Should be a Data Frame that represents the second set of individual statistics or defensive individual statistics of the players. It has to be in the format provided by the individuals_data_adjustment() function
#' @details The function will work correctly when the name of the players is the same, in case it is different it will take the players as different.
#' @return Data frame with the sum of the statistics of the other two entered data.frame.
#' @examples
#'
#' df1 <- data.frame("name" = c("LeBron James","Team"), "G" = c(67,0),
#' "GS" = c(62,0), "MP" = c(2316,0), "FG" = c(643,0), "FGA" = c(1303,0),
#' "Percentage FG" = c(0.493,0), "3P" = c(148,0),  "3PA" = c(425,0),
#' "Percentage 3P" = c(0.348,0),  "2P" = c(495,0), "2PA" = c(878,0),
#' "Percentage 2P" = c(0.564,0), "FT" = c(264,0), "FTA FG" = c(381,0),
#' "Percentage FT" = c(0.693,0), "ORB" = c(66,0),  "DRB" = c(459,0),
#' "TRB" = c(525,0),  "AST" = c(684,0),  "STL" = c(78,0),  "BLK" = c(36,0),
#' "TOV" = c(261,0),  "PF" = c(118,0),  "PTS" = c(1698,0),  "+/-" = c(0,0))
#'
#' df2 <- data.frame("name" = c("LeBron James","Team"), "G" = c(67,0),
#' "GS" = c(62,0), "MP" = c(2316,0), "FG" = c(643,0), "FGA" = c(1303,0),
#' "Percentage FG" = c(0.493,0), "3P" = c(148,0),  "3PA" = c(425,0),
#' "Percentage 3P" = c(0.348,0),  "2P" = c(495,0), "2PA" = c(878,0),
#' "Percentage 2P" = c(0.564,0), "FT" = c(264,0), "FTA FG" = c(381,0),
#' "Percentage FT" = c(0.693,0), "ORB" = c(66,0),  "DRB" = c(459,0),
#' "TRB" = c(525,0),  "AST" = c(684,0),  "STL" = c(78,0),  "BLK" = c(36,0),
#' "TOV" = c(261,0),  "PF" = c(118,0),  "PTS" = c(1698,0),  "+/-" = c(0,0))
#'
#' individuals_games_adder(df1,df2)
#'
#' @export
#'
#' @importFrom stats aggregate

individuals_games_adder <- function(df1,df2){
  if(ncol(df1) == 26 & ncol(df2) == 26){
    t1 <- df1[nrow(df1),]
    t2 <- df2[nrow(df2),]
    df1<-df1[1:(nrow(df1)-1),]
    df2<-df2[1:(nrow(df2)-1),]
    adder <- rbind(df1,df2)
    team <- rbind(t1,t2)

    names(adder) <- c("Name","G","GS","MP","FG","FGA","FG%","TP","TPA","3P%","TWP","TWPA","2P%","FT","FTA","FT%",
                        "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","P","PM")
    names(team) <- c("Name","G","GS","MP","FG","FGA","FG%","TP","TPA","3P%","TWP","TWPA","2P%","FT","FTA","FT%",
                      "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","P","PM")
    adder <- aggregate(cbind(adder$G,adder$GS,adder$MP,adder$FG,adder$FGA,adder$TP,adder$TPA,adder$TWP,adder$TWPA,adder$FT,adder$FTA,adder$ORB,
                             adder$DRB,adder$TRB,adder$AST,adder$STL,adder$BLK,adder$TOV,adder$PF,adder$P,adder$PM),
                       by=list(Name=adder$Name), FUN=sum)
    team <- aggregate(cbind(team$G,team$GS,team$MP,team$FG,team$FGA,team$TP,team$TPA,team$TWP,team$TWPA,team$FT,team$FTA,team$ORB,
                             team$DRB,team$TRB,team$AST,team$STL,team$BLK,team$TOV,team$PF,team$P,team$PM),
                       by=list(Name=team$Name), FUN=sum)
    adder <- rbind(adder,team)
    PFG<-sample(c(round(adder[5]/adder[6],3)), size = 1, replace = TRUE)
    P3P<-sample(c(round(adder[7]/adder[8],3)), size = 1, replace = TRUE)
    P2P<-sample(c(round(adder[9]/adder[10],3)), size = 1, replace = TRUE)
    PFT<-sample(c(round(adder[11]/adder[12],3)), size = 1, replace = TRUE)
    adder <- cbind(adder, PFG,P3P,P2P,PFT)
    adder <- subset (adder, select=c(1,2,3,4,5,6,23,7,8,24,9,10,25,11,12,26,13,14,15,16,17,18,19,20,21,22))
    adder[is.na(adder)] <- 0
    names(adder) <- c("Name","G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                      "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
  }else if (ncol(df1) == 12 & ncol(df2) == 12){
    t1 <- df1[nrow(df1),]
    t2 <- df2[nrow(df2),]
    df1<-df1[1:(nrow(df1)-1),]
    df2<-df2[1:(nrow(df2)-1),]
    adder <- rbind(df1,df2)
    team <- rbind(t1,t2)
    names(adder) <- c("Name","MP","DREB","FM","BLK","TOTALFM","FTO","STL","TOTALFTO","FFTA","DFGM","DFTM")
    names(team) <- c("Name","MP","DREB","FM","BLK","TOTALFM","FTO","STL","TOTALFTO","FFTA","DFGM","DFTM")
    adder <- aggregate(cbind(adder$MP,adder$DREB,adder$FM,adder$BLK,adder$TOTALFM,adder$FTO,adder$STL,adder$TOTALFTO,adder$FFTA,adder$DFGM,adder$DFTM), by=list(Name=adder$Name), FUN=sum)
    team <- aggregate(cbind(team$MP,team$DREB,team$FM,team$BLK,team$TOTALFM,team$FTO,team$STL,team$TOTALFTO,team$FFTA,team$DFGM,team$DFTM), by=list(Name=team$Name), FUN=sum)
    adder <- rbind(adder,team)
    names(adder) <- c("Name","MP","DREB","FM","BLK","TOTALFM","FTO","STL","TOTALFTO","FFTA","DFGM","DFTM")
    adder[is.na(adder)] <- 0
  }
  adder[is.na(adder)] <- 0
  return(adder)
}
