\name{bubbles-methods}
\docType{methods}
\alias{bubbles}
\alias{bubbles-methods}
\alias{bubbles,FLQuant-method}
\alias{bubbles,FLQuants-method}
\alias{bubbles,formula,FLQuant-method}
\alias{bkey}
\alias{bkey-methods}
\alias{bkey,list-method}
\title{Lattice style bubble plots}
\description{
Use this method to do plots with circles proportional to the mean of the data.
}
\section{Generic Function}{
\describe{
	\item{call}{bubbles(formula, data, ...}
	\item{formula}{A formula with the dependent, independent and conditional variables.}
	\item{data}{The object with the data you want to plot.}
}
}
\section{Methods}{
\describe{
	\item{signature("formula", "FLQuant", ...)}{
	 The formula must make use of the FLQuant object dimnames. There's an extra argument \code{bub.scale} that will be multiplied be the \code{pch.cex} argument, so increasing the contrast among the circles.    
	}
	\item{signature("formula", "FLQuants", ...)}{
	 The formula must make use of the FLQuant object dimnames. There's an extra argument \code{bub.scale} that will be multiplied be the \code{pch.cex} argument, so increasing the contrast among the circles. This method plots bubbles of different color for negative and positive values, controled by the argument \code{bub.col} which is a character vector with 2 values. The legend is drawn by the method \code{bkey}. Try getMethod("bkey") to look at it.   
	}
}}
\seealso{
  \code{\link{xyplot}}, \code{\link{FLQuant-class}}
}
\author{
	The FLCore Team
}
\keyword{methods}
\examples{
data(ple4sex)
# let's see how it looks
bubbles(age~year|unit, data=ple4sex@catch.n/1000, bub.scale=5)
# what about the log scale ?
bubbles(age~year|unit, data=log(ple4sex@catch.n))
}
