#' @title individual statistics calculator per minutes
#' @description The function allows the calculation of the statistics per game projected to M minutes.
#' @param df1 Should be a Data Frame that represents the individual statistics of the players. The parameter has to be in the format provided by the data_adjustment() function.
#' @param m Should be a number. This parameter has to be the number of minutes to which you want to project the statistics.
#' @details The statistical projection is made from the relationship between the number of minutes entered and the number of minutes played by the player.
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with statistics by game projected to the minutes entered.
#' @examples
#'
#'
#' df1 <- data.frame("name" = c("LeBron James","Team"),"G" = c(67,0),
#' "GS" = c(62,0),"MP" = c(2316,0),"FG" = c(643,0), "FGA" = c(1303,0),
#' "Percentage FG" = c(0.493,0),"3P" = c(148,0),"3PA" = c(425,0),
#' "Percentage 3P" = c(0.348,0),"2P" = c(495,0),"2PA" = c(878,0),
#' "Percentage 2P" = c(0.564,0),"FT" = c(264,0),"FTA FG" = c(381,0),
#' "Percentage FT" = c(0.693,0), "ORB" = c(66,0),"DRB" = c(459,0),
#' "TRB" = c(525,0),"AST" = c(684,0),"STL" = c(78,0),"BLK" = c(36,0),
#' "TOV" = c(261,0),"PF" = c(118,0),"PTS" = c(1698,0),"+/-" = c(0,0))
#'
#' m <- 48
#'
#' individuals_stats_per_minutes(df1,m)
#'
#'
#'
#' @export
#'

individuals_stats_per_minutes <- function(df1,m){
  df1 <- df1[-nrow(df1),]
  minutes<-df1[4]
  df1[4] <- df1[4]/df1[2]
  for(i in 5:ncol(df1)){
    if(i==7 || i==10 || i==13 || i==16){
      df1[i]<- round(df1[i],3)
    }
    else{
      df1[i] <- round((df1[i] / df1[2]) * (m/df1[4]),2)
    }

  }
  df1[4] <- minutes
  names(df1) <- c("Name","G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","FT","FTA","FT%",
                  "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-")
  df1[is.na(df1)] <- 0
  return(df1)
}
