\name{as.FLBiol}
\alias{as.FLBiol}
\alias{as.FLBiol-methods}
\alias{as.FLBiol,FLBiol-method}
\title{Method for creating an FLBiol object from other classes}
\description{
	Objects of class \code{\link{FLBiol-class}} can be created by coercing objects of oter classes.
	Current implementation accepts another FLBiol object only.
}
\usage{
as.FLBiol(object, ...)
}
\arguments{
  \item{object}{An FLBiol object}
  \item{\dots}{Arguments containing slots to be modified}
}
\value{
	an object of class \code{\link{FLBiol-class}}
}
\author{FLR Team. L.T. Kell}
\seealso{\code{\link{FLBiol-class}}}
\examples{
}
\keyword{methods}
