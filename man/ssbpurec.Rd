\name{ssbpurec}
\alias{ssbpurec}
\title{Method for calculating Spawning Stock Biomass per unit recruit}
\description{
	For an \code{\link{FLStock-class}} object with the natural mortality, maturity and 
	stock-weight information, this method calculates the SSB per unit recruit.
}
\usage{
ssbpurec(object, ...)
}
\arguments{
  \item{object}{ An object of type \code{\link{FLStock-class}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
        The SSB per unit recruit is a factor in stock-recruit functions, and yield-per-recruit 
		analyses, and can be computed either from regular \code{FLStock} information 
		(maturity, natural mortality, stock weights), or from a simple regression 
		of the gradient of the equilibrium SSB-Recruits curve with zero fishing mortality. 
		Either are permitted, by using either 'non-param' or 'param' in the \emph{type} 
		argument in the method. 
		
		For the non-parametric method, we can also supply arguments to tell the method when 
		to compute yearly-averaged values of natural mortality, weights and maturity, if 
		we believe that a certain subset of the years is clearly representative of virgin 
		conditions - for whatever reason - and this is done using the \emph{start} and 
		\emph{end} arguments and \code{window}. The default behaviour is to just use the first 
		year in the object. Also, if we have a seasonal model, the user can supply 
		recruitment (\emph{recs}) and spawning (\emph{spwns}) seasons, so they can better 
		represent their stock's biology when computing the quantity - the default is that 
		both happen in season one.
}
\value{
	An object of class \code{\link{FLQuant-class}} whose first dimension is of length one.
}
\references{}
\author{FLR Team}
\note{}
\seealso{\code{\link{FLStock-class}}}
\examples{\dontrun{
data(ple4)
spur.ple4 <- ssbpurec(ple4,start=1957,end=1966,type='non-param',plusgroup=FALSE)
}
}
\keyword{methods}
