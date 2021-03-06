% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineups_backcourt.R
\name{lineups_backcourt}
\alias{lineups_backcourt}
\title{Statistics searcher of backcourt players}
\usage{
lineups_backcourt(df1)
}
\arguments{
\item{df1}{Should be a Data Frame. The parameter has to be in the format provided by the lineups_data_adjustment() function.}
}
\value{
Data frame with the statistics of the backcourt players
}
\description{
The function allows find the statisticts of backcourt players
}
\details{
The function works with the basic statistics of the lineups and the extended statistics of the lineups.
}
\examples{

df1 <- data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(4,0),
"FGA " = c(7,0),"Percentage FG" = c(0.571,0),
"X3P  " = c(0,0),"X3PA  " = c(2,0),"Percentage 3P" = c(0,0),
"X2P " = c(4,0), "X2PA " = c(5,0), "Percentage 2P" = c(0.8,0),
"FT " = c(1,0), "FTA " = c(3,0), "Percentage FT" = c(0.333,0),
"ORB " = c(2,0), "DRB " = c(5,0),"TRB " = c(7,0), "AST " = c(2,0),
"STL " = c(1,0), "BLK " = c(0,0),"TOV " = c(7,2), "PF" = c(1,0),
"PLUS" = c(9,0),"MINUS" = c(17,3),"P/M" = c(-8,-3))

lineups_backcourt(df1)

df1 <-  data.frame("PG" = c("James","Rondo"),"SG" = c("Green","Caruso"),
"SF" = c("Caldwell","Kuzma"), "PF" = c("Davis","Davis"),
"C" = c("Howard ","Howard"),"MP" = c(7,1), "FG " = c(6,0),
"OppFG " = c(6,0), "FGA " = c(10,0),"OppFGA " = c(9,0),
"X3P  " = c(2,0),"Opp3P" = c(1,0),"X3PA" = c(4,0),"Opp3PA" = c(3,0),
"X2P" = c(4,0),"Opp2P " = c(5,0), "X2PA " = c(6,0),"Opp2PA " = c(8,0) ,
"FT " = c(0,0),"OppFT " = c(1,0), "FTA " = c(0,0),"OppFTA " = c(1,0),
"OppRB " = c(2,0),"OppOppRB " = c(1,0), "DRB" = c(4,0),"OppDRB" = c(1,0),
"TRB" = c(6,0),"OppTRB" = c(2,0), "AST " = c(5,0),"OppAST " = c(4,0),
"STL " = c(1,0),"OppSTL " = c(3,0), "BLK " = c(0,0),  "OppBLK " = c(1,0),
"TOppV " = c(5,2), "OppTOppV " = c(3,2),"PF" = c(1,0),"OppPF" = c(3,0),
"PLUS" = c(15,0),"MINUS" = c(14,3),"P/M" = c(1,-3))

lineups_backcourt(df1)

}
\author{
Fco Javier Cantero \email{fco.cantero@edu.uah.es}

Juan José Cuadrado \email{jjcg@uah.es}

Universidad de Alcalá de Henares
}
