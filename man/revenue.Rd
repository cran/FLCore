\name{revenue}
\docType{methods}
\alias{revenue}

\title{Calculate the revenue of a fleet}
\description{
  This is a method that calculates the gross or nett revenue for an object. It is currently only defined
  for objects of class FLFleet, but may in the future be extended to other classes. 

  The revenue is calculated as by the last 4 dimensions of the FLQuants used, because mixing of quants
  is allowed in FLFleet. The type of revenue (either gross or nett revenue) to be calculated is set by
  the "type" argumement. Gross revenue is calculated by the sum of landings.n*landings.wt*price slots 
  in each of the FLCatch objects present within the fleet. Nett revenue is calculated by substracting
  the sum of the costs in the fleet from the gross revenue. 
}

\value{
  An FLQuant with length 1 in the first dimension
}

\author{FLR Team}

\seealso{
 \code{\link{FLFleet-class}}
}


\examples{
 data(bt4)
 bt4@catches[[1]]@price[] <- 3
 bt4@catches[[2]]@price[] <- 6
 revenue(bt4, type="gross")
}

\keyword{methods}