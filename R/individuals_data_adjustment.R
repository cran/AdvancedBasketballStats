#' @title Individual stat adjuster
#' @description The function transform the statistics entered for later use in the rest of the functions that apply to individuals statistics.
#' @param df1 Should be a Data Frame that represents the individual statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @details \itemize{
#'                 \item The data.frame must have the same columns and these represent the same as in the example.
#'                 \item The input data.frame must have the last row that represents the team's statistics.
#'                 \item The function allows the transformation of the individual's statistics to which the shooting percentages and the number of total rebounds are added.
#'                 \item The function allows the transformation of the defensive statistics to which the force missed shot and the forced turnovers.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data.frame with the transformed statistics for use in the rest of the functions.
#' @return The data frame obtained for the individual's statistics will have the following format:
#'         \itemize{
#'                 \item Name of the player (Name)
#'                 \item Games played (G)
#'                 \item Games Started (GS)
#'                 \item Minutes Played (MP)
#'                 \item Field Goals Made (FG)
#'                 \item Field Goals Attempted (FGA)
#'                 \item Field Goals Percentage (FG\%)
#'                 \item Three Points Made (3P)
#'                 \item Three Points Attempted (3PA)
#'                 \item Three Points Percentage (3P%)
#'                 \item Two Points Made (2P)
#'                 \item Two Points Attempted (2PA)
#'                 \item Two Points Percentage (2P\%)
#'                 \item Free Throw Made (FT)
#'                 \item Free Throw Attempted (FTA)
#'                 \item Free Throw Percentage (FT\%)
#'                 \item Offensive Rebounds (ORB)
#'                 \item Defensive Rebounds (DRB)
#'                 \item Total Rebounds (TRB)
#'                 \item Assists (AST)
#'                 \item Steals (STL)
#'                 \item Blocks (BLK)
#'                 \item Turnover (TOV)
#'                 \item Personal Fouls (PF)
#'                 \item Points (PTS)
#'                 \item Plus Minus (+/-)
#'         }
#' The data frame obtained for the defensive individual's statistics will have the following format:
#'         \itemize{
#'                 \item Name of the player (Name)
#'                 \item Minutes Played (MP)
#'                 \item Defensive Rebounds (DRB)
#'                 \item FGA by opposing team (FM)
#'                 \item Blocks (BLK)
#'                 \item  (TOTAL FM)
#'                 \item Forced turnover(FTO)
#'                 \item Steals (STL)
#'                 \item Total forced turnover (TOTAL FTO)
#'                 \item FTA by opposing team (FFTA)
#'                 \item FG made by opposing team (DFGM)
#'                 \item FT made by opposing team (DFTM)
#'         }
#' @examples

#' df1 <- data.frame("Name" = c("James","Team"), "G" = c(67,0), "GS" = c(62,0),
#' "MP" = c(2316,1), "FG" = c(643,0), "FGA" = c(1303,0),"3P  " = c(148,0),
#' "3PA" = c(425,0),"2P" = c(495,0), "2PA" = c(878,0),  "FT" = c(264,0),
#' "FTA" = c(381,0),"ORB" = c(66,0),  "DRB" = c(459,0), "AST" = c(684,0),
#' "STL" = c(78,0),  "BLK" = c(36,0),"TOV" = c(261,0),  "PF" = c(118,0),
#' "PTS" = c(1698,0),  "+/-" = c(0,0))
#'
#' individuals_data_adjustment(df1)
#'
#' df2 <- data.frame("Name" = c("Witherspoon ","Team"), "MP" = c(14,200),
#' "DREB" = c(1,0),"FM" = c(4,0), "BLK" = c(0,0),"FTO" = c(0,0),
#' "STL" = c(1,1), "FFTA" = c(0,0),  "DFGM" = c(1,0), "DFTM" = c(0,0))
#'
#' individuals_data_adjustment(df2)
#'
#' @export
#'

individuals_data_adjustment <- function(df1){
  if(ncol(df1)==21){
    PFG <- sample(c(round(df1[5]/df1[6],3)), size = 1, replace = TRUE)
    P3P <- sample(c(round(df1[7]/df1[8],3)), size = 1, replace = TRUE)
    P2P <- sample(c(round(df1[9]/df1[10],3)), size = 1, replace = TRUE)
    PFT <- sample(c(round(df1[11]/df1[12],3)), size = 1, replace = TRUE)
    TRB <- sample(c(df1[13]+df1[14]), size = 1, replace = TRUE)
    df1 <- cbind(df1, PFG,P3P,P2P,PFT,TRB)
    data_adjustment <- subset (df1, select=c(1,2,3,4,5,6,22,7,8,23,9,10,24,11,12,25,13,14,26,15,16,17,18,19,20,21))
    names(data_adjustment) <- c("Name","G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                                "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
    data_adjustment[is.na(data_adjustment)] <- 0

  }  else if(ncol(df1)==10){
    names(df1) <- c("Name","MP","DREB","FM","BLK","FTO","STL","FFTA","DFGM","DFTM")
    totalfm<- df1[4]+df1[5]
    totalfto <- df1[6]+df1[7]
    data_adjustment <- cbind(df1,totalfm,totalfto)
    data_adjustment <- subset (data_adjustment, select=c(1,2,3,4,5,11,6,7,12,8,9,10))
    names(data_adjustment) <- c("Name","MP","DRB","FM","BLK","TOTAL FM","FTO","STL","TOTAL FTO","FFTA","DFGM","DFTM")
    data_adjustment[is.na(data_adjustment)] <- 0
  }
  data_adjustment[is.na(data_adjustment)] <- 0
  return(data_adjustment)
}
