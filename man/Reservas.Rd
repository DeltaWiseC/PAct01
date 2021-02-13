\name{Reservas}
\alias{Reservas}
\title{Calculo de Reservas}
\description{
Determina el monto de las reservas para un un seguro de vida
}
\usage{
Reservas(Edad, Tabla, Tasa, PlazoSeg, PlazoPag = 0, Dotal = 0, Fsel = 0, Persis = 0, PrimaRiesgo)
}

\arguments{
  \item{Edad}{Edad de calculo}
  \item{Tabla}{Tabla de mortalidad}
  \item{Tasa}{Tasa de interes de calculo, puede recibir una curva de tasas}
  \item{PlazoSeg}{Plazo del seguro}
  \item{PlazoPag}{Plazo de Pago}
  \item{Dotal}{Indicador si es un plan dotal 0-No  1-Si}
  \item{Fsel}{Factores de selección }
  \item{Persis}{Supuesto de Persistencia}
  \item{PrimaRiesgo}{Monto de la prima de riesgo}
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Martin Quezada}
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
