\name{PTarifa}
\alias{PTarifa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prima de Tarifa / Al cobro
}
\description{
Calcula la prima de tarifa
}
\usage{
PTarifa(Edad,Tabla,Tasa,PlazoSeg,PlazoPag,Dotal,Fsel,Persis,Gvar,Gfij,Comis,Sap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{Edad}{Edad de calculo de la prima}
  \item{Tabla}{Tabla de mortalidad a usar 2-   3-CNSF2000I}
  \item{Tasa}{Tasa de interes tecnico, puede recibir una curva de tasas, considera años}
\item{PlazoSeg}{Plazo de Seguro}
\item{PlazoPag}{Plazo de Pago}
\item{Dotal}{Indicador se el plan es dotal, 1-si 0-no  por default 0}
\item{Fsel}{Factores de selección  0 no  1-c(.6,.85)  2-c(.75,.90)   3-c(.8,.95)}
\item{Persis}{Factores de persistencia   0- no   1-c(1,.75,.85,0.90,0.92,0.94,0.96) 2-c(1,.70,.80,0.85,0.90,0.92,0.95)  3-c(1,.80,.88,0.92,0.94,0.96,0.98)}
\item{Gvar}{Gasto variable, en porcentaje de la prima}
\item{Gfij}{Gasto Fijo, entantos al millar con base a la suma asegurada promedio}
\item{Comis}{Esquema de comisiones}
\item{Sap}{Suma Asegurada Promedio}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Regresa la prima de tarifa considerando los gastos nivelados en el perido de pago de primas
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
"Martin Quezada"}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
