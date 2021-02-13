\name{PRiesgo}
\alias{PRiesgo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prima de Riesgo
%%  ~~function to do ... ~~
}
\description{
Determina la prima de riesgo de un seguro de vida para una edad determinada
}
\usage{
PRiesgo(Edad,Tabla,Tasa,PlazoSeg,PlazoPag,Dotal,Fsel,Persis)
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
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Regresa la prima de tarifa ajustada por (i/ delta)
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
"Martin Quezada"
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

PRiesgo(Edad=30,Tabla=2,Tasa=0.05,PlazoSeg=20,PlazoPag=0,Dotal=0,Fsel=0,Persis=0)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
