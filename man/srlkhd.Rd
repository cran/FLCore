% $ Id: $
\name{srlkhd}
\alias{srlkhd}
\title{Likelihood of the S-R parameters}
\description{
	The \code{srlkhd} function computes the likelihood of the parameters
	in the FLSR object.
}
\usage{
	srlkhd(x)
}
\arguments{
  \item{x}{An FLSR object holding the stock and recruitment data (slots r and ssb) and parameter estimates.}
}
\details{
	This function returns the model type, the parameter estimates and the log-likelihood of the FLSR 
	object passed to it. Primary use is for establishing a sensible initial guess for when using the \code{sr}
 	method to fit the S-R model.
   }
\value{
    The S-R model, the parameter estimates and the log-likelihood for those parameters and that model.
}
\references{}
\author{Richard Hillary and Iago Mosqueira}
\note{}
\seealso{\code{\link{FLSR-class}}}
\examples{
}
\keyword{models}
