% $Id: is.FLIndex.Rd,v 1.1.2.3 2006/03/13 12:12:14 iagoazti Exp $
\name{is.FLIndex}
\alias{is.FLIndex}
\title{FLIndex}
\description{Testing for the class of FLIndex objects}
\usage{
  is.FLIndex(x)
}
\arguments{
  \item{x}   {a \code{FLIndex} object or any other object to test.}
}
\value{
	\code{is.FLIndex} returns \code{TRUE} or \code{FALSE} depending on whether the object is or not of class \code{FLIndex}.
}
\seealso{
    \code{\link{FLIndex-class}}
}
\examples{
	# This is TRUE
	is.FLIndex(FLIndex())
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
