% $Id: trim.Rd,v 1.2.2.1 2005/12/19 11:13:48 iagoazti Exp $
\docType{methods}
\name{trim}
\alias{trim}
\alias{trim-methods}
\title{Method for trimming FLQuant objects.}
\description{
This method provides a trimming procedure for FLQuants. This is a generic method, currently defined
for FLQuant, and can be overloaded for other classes.
}
\usage{
	trim(object, ...)
}
\arguments{
	\item{object}{The object to be trimmed}
	\item{...}{dimensions and dimnames to trim the object along.}
}
\section{Methods}{
	\describe{
		\item{signature(FLQuant)}{trim for FLQuant object}
	}
}
\examples{
# load ple4
data(ple4)
# subset catches
trim(ple4@catch.n, age=2:7, year=1970:1980)
# now with unit dimension
example("FLQuant")
trim(sq, unit="female")
}
\seealso{\code{\link{subset}}
}
\author{
	FLR Team
}
\keyword{methods}
