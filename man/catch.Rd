\name{catch}
\alias{computeCatch}
\alias{computeLandings}
\alias{computeDiscards}
\alias{computeCatch-methods}
\alias{computeLandings-methods}
\alias{computeDiscards-methods}
\alias{computeCatch,FLStock-method}
\alias{computeCatch,FLIndex-method}
\alias{computeLandings,FLStock-method}
\alias{computeDiscards,FLStock-method}
\title{Methods for estimating aggregated catch time series from dissagregated data}
\description{
	Object such as \code{\link{FLStock-class}} that incorporate quant-dissagregated objects for catch, diacards and landings nunber and weights, usually have quant-aggregated versions. To estimate the later from the first two, these methods (\code{computeCatch}, \code{computeLandings} and \code{computeDiscards}) can be used.
}
\usage{
computeCatch(object, ...)
computeLandings(object, ...)
computeDiscards(object, ...)
}
\arguments{
  \item{object}{An object containing slots for numbers-by-quant and weight-at-quant. Currently object can be of class \code{\link{FLStock-class}} or \code{\link{FLCatch-class}}.}
  \item{\dots}{Extra arguments accepted by each implementation}
}
\details{
        Sums catch numbers at age multiplied by catch weights at age within a year to give total catch. Total 
        landings and total discards are calculated in a similar fashion. The method returns an \code{\link{FLQuant-class}} 
        with first dimension of length one. 

        The catch method has an additional arguement "overwrite". With the default setting of FALSE, the method returns an 
        \code{\link{FLQuant-class}}. If overwrite=TRUE the method will return an \code{\link{FLStock-class}} for which the catch.n, 
        catch.wt and catch slots have been filled. catch.n is calculated as the sum of landings.n and discards.n. catch.wt is a 
        weighted average of landings.wt and discards.wt.
        
        On creation of a new \code{\link{FLStock-class}} object, discards.n and discards.wt slots are set to 'NA' by default. If discards data are not available the user should specify 
        zero values (ie. stock@discards.n[,,,,] <- 0 ) otherwise catch and discards methods will return 'NA' values.
}
\value{
	An object of class \code{\link{FLQuant-class}} whose first dimension is of length one.
}
\references{}
\author{FLR Team}
\note{}
\seealso{\code{\link{FLStock-class}}, \code{\link{catch,FLStock-method}}}
\keyword{methods}
