% $ Id: $
\name{steepvirginbiom}
\alias{steepvirginbiom}
\title{Change to steepness/virgin biomass parameterisation}
\description{
	The \code{steepvirginbiom} function converts the Beverton-Holt and Ricker 
	model standard parameterisations into the steepness/virgin biomass form, given the 
	FLSR object and the SSB per unit recruit.
}
\usage{
	steepvirginbiom(object,ssbperunitrec)
}
\arguments{
  \item{object}{An FLSR object holding the stock and recruitment data (slots r and ssb) and parameter estimates.}
  \item{ssbperunitrec}{The SSB per unit recruit.}
}
\details{
	This function returns a list with the steepness and virgin biomass estimates for the given 
	FLSR object, using the SSB per unit recruit and only for the Ricker and Beverton-Holt models.
   }
\value{
    A list with the transformed parameters.
}
\references{}
\author{Richard Hillary and Iago Mosqueira}
\note{}
\seealso{\code{\link{FLSR-class}}}
\examples{
}
\keyword{models}
