\docType{methods}
\name{tofrm}
\alias{tofrm}
\alias{tofrm-methods}
\title{Method for generating fromulas from FLQuant objects.}
\description{
This method provides a formula object based on the dimensions and dimnames of the object.
}
\usage{
	tofrm(object, ...)
}
\arguments{
	\item{object}{The object to be trimmed}
	\item{...}{Whatever}
}
\section{Methods}{
	\describe{
		\item{signature(FLQuant)}{Formulas for FLQuant.}
	}
}
\examples{
# load ple4
data(ple4)
tofrm(catch.n(ple4))

}
\seealso{\code{\link{formula}}
}
\author{
	FLR Team
}
\keyword{methods}
