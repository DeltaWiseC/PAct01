\name{CashFlow}
\alias{CashFlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Proyección de Cash Flow para una póliza
}
\description{Determina el flujo de efectivo año por año para una poliza}
\usage{
CashFlow(Veckpx,Vecqx,Vecper,tasa,Plazo,Prima,Veccom,Gadminfijo,Gadminvar,SumaAseg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{Veckpx}{Vector de probabilidades de sobrevivencia}
 \item{Vecqx}{Vector de probabilidades de muerte}
 \item{Vecper}{Vector de persistencia}
 \item{tasa}{tasa de interes }
 \item{Plazo}{Plazo de seguro}
 \item{Prima}{Prima a pagar}
 \item{Veccom}{Vector de comisiones}
 \item{Gadminfijo}{Gasto de administración fijo}
 \item{Gadminvar}{Gasto de administración variabe}
 \item{SumaAseg}{Suma Asegurada}
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
"MArtin Quezada"
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
