#' @title Statistics searcher
#' @description The function allows the statistical search of the lineups where the entered players appear.
#' @param df1 Should be a Data Frame. This parameter has to be in the format provided by the lineups_advance_stats() function.
#' @param n Should be a numer. It represents the number of player to be found.
#' @param p1 Should be a String. Represents the name of the first player to be found.
#' @param p2 Should be a String. Represents the name of the second player to be found.
#' @param p3 Should be a String. Represents the name of a player to be found.
#' @param p4 Should be a String. Represents the name of a player to be found.
#' @details \itemize{
#'                 \item The function allows you to search for paint players both in basic statistics and in extended statistics.
#'                 \item The values allowed by n are 1, 2, 3 and 4.The number entered in N must be equal to the number of players searched.
#'                 \item The name entered in the function must be the same as the one inside the data frame.
#'         }
#' @author Fco Javier Cantero \email{fco.cantero@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Data frame with the statistics of the lineups where the entered players appear
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
#' n <- 2
#'
#' p1 <- "James"
#'
#' p2 <- "Davis"
#'
#' p3 <- ""
#'
#' p4 <- ""
#'
#'
#' lineups_searcher(df1,n,p1,p2,p3,p4)
#'
#' @export
#'

lineups_searcher <- function(df1,n,p1,p2,p3,p4){
  if(n==1){
    aux2 <- df1[df1$PG==p1,]; aux3 <- df1[df1$SG==p1,]; aux4 <- df1[df1$SF==p1,]; aux5 <- df1[df1$PF==p1,]; aux6 <- df1[df1$C== p1,]
    df<-rbind(aux2,aux3,aux4,aux5,aux6)
  }else if(n==2){
    aux2 <- df1[df1$PG==p1,]; aux3 <- df1[df1$SG==p1,]; aux4 <- df1[df1$SF==p1,]; aux5 <- df1[df1$PF==p1,]; aux6 <- df1[df1$C== p1,]
    aux1<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux1[aux1$PG==p2,]; aux3 <- aux1[aux1$SG==p2,];aux4 <- aux1[aux1$SF==p2,]; aux5 <- aux1[aux1$PF==p2,]; aux6 <- aux1[aux1$C==p2,]
    df<-rbind(aux2,aux3,aux4,aux5,aux6)
  }else if (n==3){
    aux2 <- df1[df1$PG==p1,]; aux3 <- df1[df1$SG==p1,]; aux4 <- df1[df1$SF==p1,]; aux5 <- df1[df1$PF==p1,]; aux6 <- df1[df1$C== p1,];
    aux1<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux1[aux1$PG==p2,]; aux3 <- aux1[aux1$SG==p2,]; aux4 <- aux1[aux1$SF==p2,]; aux5 <- aux1[aux1$PF==p2,]; aux6 <- aux1[aux1$C==p2,]
    aux<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux[aux$PG==p3,]; aux3 <- aux[aux$SG==p3,]; aux4 <- aux[aux$SF==p3,]; aux5 <- aux[aux$PF==p3,]; aux6 <- aux[aux$C==p3,]
    df<-rbind(aux2,aux3,aux4,aux5,aux6)
  }else if(n==4){
    aux2 <- df1[df1$PG==p1,]; aux3 <- df1[df1$SG==p1,]; aux4 <- df1[df1$SF==p1,]; aux5 <- df1[df1$PF==p1,]; aux6 <- df1[df1$C== p1,]
    aux1<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux1[aux1$PG==p2,]; aux3 <- aux1[aux1$SG==p2,]; aux4 <- aux1[aux1$SF==p2,]; aux5 <- aux1[aux1$PF==p2,]; aux6 <- aux1[aux1$C==p2,]
    aux<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux[aux$PG==p3,]; aux3 <- aux[aux$SG==p3,]; aux4 <- aux[aux$SF==p3,]; aux5 <- aux[aux$PF==p3,]; aux6 <- aux[aux$C==p3,]
    aux<-rbind(aux2,aux3,aux4,aux5,aux6)
    aux2 <- aux[aux$PG==p4,]; aux3 <- aux[aux$SG==p4,]; aux4 <- aux[aux$SF==p4,]; aux5 <- aux[aux$PF==p4,]; aux6 <- aux[aux$C==p4,]
    df<-rbind(aux2,aux3,aux4,aux5,aux6)
  }
  return(df)
}


