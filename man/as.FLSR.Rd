% $Id: as.FLSR.Rd,v 1.1.2.2 2006/10/19 15:40:36 iagoazti Exp $
\name{as.FLSR}
\alias{as.FLSR}
\alias{as.FLSR,FLSR-method}
\alias{as.FLSR,FLSR-method}
\title{Method for creating an FLSR object from other classes.}
\description{
    Objects of class \code{\link{FLBiol-class}} can be created by coercing objects of other classes.
	Current implementation accepts 'FLBiol' and 'FLStock' objects.}
\usage{
  as.FLSR(object, ...)
}
\arguments{
  \item{object} {an 'FLBiol' or 'FLStock' object to convert to \code{FLSR}.}
  \item{...}{Any other 'FLSR' slot}
}
\details{
    This is a generic method. New methods can be defined to convert various classes to \code{FLSR}.
    Methods exist for conversion of \code{FLStock} and \code{FLBiol} objects.
}
\value{
	\code{as.FLSR} returns an \code{FLSR} object
}
\seealso{
    \code{\link{FLSR-class}}, \code{\link{FLBiol-class}}, \code{\link{FLStock-class}}.
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
