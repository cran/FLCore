\name{FLCore lattice-methods}
\docType{methods}
\alias{xyplot,FLQuant-method}
\alias{xyplot,FLQuants-method}
\alias{bwplot,FLQuant-method}
\alias{dotplot,FLQuant-method}
\alias{barchart,FLQuant-method}
\alias{stripplot,FLQuant-method}
\alias{histogram,FLQuant-method}
\alias{xyplot,formula-method}
\alias{xyplot,formula-method}
\alias{bwplot,formula-method}
\alias{dotplot,formula-method}
\alias{barchart,formula-method}
\alias{stripplot,formula-method}
\alias{histogram,formula-method}
\title{FLCore lattice methods}
\description{These are implementations of the most common lattice plots to FLQuant objects.}
\section{Generic Function}{
Lattice is implemented in S3 and the S4 generic functions were defined by us. The mechanism is quite ugly and the implementations are terrible hacks, although they work in the end (nobody really knows why ...). The main side effect is that the generic is defined for a \code{formula} object although it is suppose to act based on the FLQuant object that is feeded with the \code{data} argument.  
\describe{
	\item{call}{xyplot(formula, ...)}
	\item{formula}{A formula with the dependent, independent and conditional variables.}
}
}
\section{Methods}{
\describe{
	\item{xyplot("formula", "FLQuant")}{see example section}
	\item{bwplot("formula", "FLQuant")}{see example section}
	\item{stripplot("formula", "FLQuant")}{see example section}
	\item{dotplot("formula", "FLQuant")}{see example section}
	\item{barchart("formula", "FLQuant")}{see example section}
	\item{histogram("formula", "FLQuant")}{see example section}
	\item{xyplot("formula", "FLQuants")}{see example section}
}	
\emph{Read the man pages for lattice methods.}
}
\details{
The implementation of these methods make use of \code{as.data.frame} to coerce FLQuant objects into data frames that can be feeded to the lattice methods. The data frame will have the same names as the dimnames and the data it self will be on a \code{data} column. The main effect of this procedure is that if one wants to call for a lattice method with a formula, as we advice, it must use \code{data~...}.}
\seealso{
  \code{\link{xyplot}}, \code{\link{FLQuant-class}, \code{\link{bubbles-methods}}}
}
\author{
	The FLCore Team
}
\keyword{methods}
\examples{
data(ple4sex)
# plot the ages per year for males
xyplot(data~age|year, data=trim(ple4sex@catch.n, unit="male"), type="b")
# make use of FLQuants to plot males and females together
plelst <- list(males=trim(ple4sex@catch.n, unit="male"), females=trim(ple4sex@catch.n, unit="female"))
flqs <- FLQuants(plelst)
xyplot(data~age|year, data=flqs, type="b", auto.key=TRUE)
# use boxplots to explore the variability along the years
bwplot(data~age|unit, data=ple4sex@catch.n)
# or just take a look at all results
dotplot(data~age|unit, data=ple4sex@catch.n)
# plot the age frequencies per year for males
barchart(data~age|year, data=trim(ple4sex@catch.n, unit="male"))
# or compare the last 10 years per sex
barchart(data~age|year*unit, data=trim(ple4sex@catch.n, year=1990:2000))
# be carefull with what you do with histogram ...
# probably it makes sense for simulation situations ...
histogram(~data|year, data=ple4sex@catch.n)
}
