\name{ssb}
\alias{ssb}
\title{Method for calculating Spawning Stock Biomass}
\description{
	For an \code{\link{FLStock-class}} object with the numbers, maturity and stock-weight information, this method calculates the SSB.
}
\usage{
ssb(object, ...)
}
\arguments{
  \item{object}{AN object of type \code{\link{FLStock-class}}.}
  \item{\dots}{Extra arguments accepted by each implementation}
}
\details{
        The nature of the calculation depends on the units in the harvest slot. If we have non-zero entries in the harvest.spwn and m.spwn 
	slots, then the SSB values will be different for harvest rates and fishing mortality scenarios.
}
\value{
	An object of class \code{\link{FLQuant-class}} whose first dimension is of length one.
}
\references{}
\author{FLR Team}
\note{}
\seealso{\code{\link{FLStock-class}}}
\keyword{methods}
