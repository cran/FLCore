% $Id: as.FLStock.Rd,v 1.4.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{as.FLStock}
\alias{as.FLStock}
\alias{as.FLStock,FLStock-method}
\alias{as.FLStock,FLFleet-method}
\title{FLStock}
\description{Convertion to FLStock objects}
\usage{
  as.FLStock(object, ...)
}
\arguments{
  \item{object} {an FLR object to convert to \code{FLStock}}
  \item{...}    {Any other options applicable to the individual method}
}
\details{
    This is a generic method. New methods can be defined to convert various classes to \code{FLStock}. Methods exist for conversion of \code{FLStock} and \code{FLFleet} objects.
}
\value{
	\code{as.FLStock} returns an \code{FLStock} object
}
\seealso{
    \code{\link{FLStock-class}}, \code{\link{FLFleet-class}}
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
