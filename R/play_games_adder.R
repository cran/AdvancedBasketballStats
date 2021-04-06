#' @title Play games adder
#' @description The function allows to perform the sums of two data.frames with the same format adopted after being transformed by play_data_adjustment() function,
#' @param df1 Should be a Data Frame that represents the first set of play's statistics. It has to be in the format provided by the play_data_adjustment() function.
#' @param df2 Should be a Data Frame that represents the second set of play's statistics. It has to be in the format provided by the play_data_adjustment() function.
#' @details The function will work correctly when the name of the players is the same, in case it is different it will take the players as different.
#' @return Data frame with the sum of the statistics of the other two entered data frame.
#' @examples
#'
#' df1 <- data.frame("Name" = c("Sabonis ","Team"), "GP" = c(62,71),
#' "PTS" = c(387,0), "FG" = c(155,1), "FGA" = c(281,1),
#' "FGA Percentage" = c(0.552,1),"3P" = c(6,1),"3PA" = c(18,1),
#' "3P Percentage" = c(0.333,1),"2P" = c(149,0),"2PA" = c(263,0),
#' "2P Percentage" = c(0.567,0),"FT" = c(39,1),  "FTA" = c(53,1),
#' "FT Percentage" = c(0.736,1),  "ANDONE" = c(12,1), "AST" = c(0,1),
#' "TOV" = c(27,1))
#'
#' df2 <- data.frame("Name" = c("Sabonis ","Team"), "GP" = c(62,71),
#' "PTS" = c(387,0), "FG" = c(155,1), "FGA" = c(281,1),
#' "FGA Percentage" = c(0.552,1),"3P" = c(6,1),"3PA" = c(18,1),
#' "3P Percentage" = c(0.333,1),"2P" = c(149,0),"2PA" = c(263,0),
#' "2P Percentage" = c(0.567,0),"FT" = c(39,1),  "FTA" = c(53,1),
#' "FT Percentage" = c(0.736,1),  "ANDONE" = c(12,1), "AST" = c(0,1),
#' "TOV" = c(27,1))
#'
#' play_games_adder(df1,df2)
#'
#'
#' @export
#'
#'
#'
#' @importFrom stats aggregate
#'

play_games_adder <- function(df1,df2){
  if (ncol(df1) == 18 & ncol(df2) == 18){
    t1 <- df1[nrow(df1),]
    t2 <- df2[nrow(df2),]
    df1<-df1[1:(nrow(df1)-1),]
    df2<-df2[1:(nrow(df2)-1),]
    adder <- rbind(df1,df2)
    team <- rbind(t1,t2)
    names(adder) <- c("Name","G","PTS","FG","FGA","FGP","TP","TPA","TPP","TWP","TWPA","TWPP","FT","FTA","FTP","ANDONE","AST","TOV")
    names(team) <- c("Name","G","PTS","FG","FGA","FGP","TP","TPA","TPP","TWP","TWPA","TWPP","FT","FTA","FTP","ANDONE","AST","TOV")
    adder <- aggregate(cbind(adder$G,adder$PTS,adder$FG,adder$FGA,adder$TP,adder$TPA,adder$TWP,adder$TWPA,adder$FT,adder$FTA,adder$ANDONE,
                             adder$AST,adder$TOV), by=list(Name=adder$Name), FUN=sum)
    team <- aggregate(cbind(team$G,team$PTS,team$FG,team$FGA,team$TP,team$TPA,team$TWP,team$TWPA,team$FT,team$FTA,team$ANDONE,
                             team$AST,team$TOV), by=list(Name=team$Name), FUN=sum)
    adder <- rbind(adder,team)
    fgp <- round(adder[4] / adder[5],3)
    tp  <- round(adder[6] / adder[7],3)
    twp <- round(adder[8] / adder[9],3)
    ftp <- round(adder[10] / adder[11],3)
    adder <- cbind(adder,fgp,tp,twp,ftp)
    adder <- subset (adder, select=c(1,2,3,4,5,15,6,7,16,8,9,17,10,11,18,12,13,14))
    names(adder) <- c("Name","GP","PTS","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%","ANDONE","AST","TOV")
    adder[is.na(adder)] <- 0
  }
  adder[is.na(adder)] <- 0
  return(adder)
}
