\name{window}
\docType{methods}
\alias{window}
\alias{window-methods}
\alias{window,FLStock-method}
\alias{window,FLIndices-method}
\alias{window,FLQuant-method}
\alias{window,FLBiol-method}
\alias{window,FLCatch-method}
\alias{window,FLFleet-method}
\alias{window,FLIndex-method}

\title{window for FLQuants based classes}

\description{Selects a section of the object based on year contraints. For composed classes it will constraint all the slots equally.}

\section{Implemented Methods}{
\describe{
\item{signature(FLStock)}{window for FLStock objects}
\item{signature(FLIndices)}{window for FLIndices objects}
\item{signature(FLQuant)}{window for FLQuant objects}
\item{signature(FLBiol)}{window for FLBiol objects}
\item{signature(FLCatch)}{window for FLCatch objects}
\item{signature(FLFleet)}{window for FLFleet objects}
\item{signature(FLIndex)}{window for FLIndex objects}
}}

\details{

The function call is

 \code{function (x, start, end, extend = TRUE, frequency = 1)}

where \code{x} is the object, \code{start} and \code{end} define the time lag, \code{extend} is a logical that forces the object to have the defined time lag fulfilling the missing values with \code{NA}, and \code{frequency} is the time interval to be extracted. 

}
\author{FLR Team}

\seealso{
 \code{\link{FLStock-class}}, \code{\link{FLIndices-class}}, \code{\link{window}} 
}

\examples{
data(ple4)
ple490 <- window(ple4, 1990, 2000)
}
\keyword{methods}
