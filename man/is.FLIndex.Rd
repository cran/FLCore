% $Id: is.FLIndex.Rd,v 1.1.2.2 2005/12/20 15:40:25 iagoazti Exp $
\name{is.FLIndex}
\alias{is.FLIndex}
\alias{is.FLIndexCom}
\alias{is.FLIndexSurvey}
\alias{is.FLIndexAcoustic}
\title{FLIndex}
\description{Testing for the class of FLIndex objects}
\usage{
  is.FLIndex(x)
  is.FLIndexCom(x)
  is.FLIndexSurvey(x)
  is.FLIndexAcoustic(x)
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

	# and this is TRUE too
	is.FLIndex(FLIndexCom())
	
	# This is clearly TRUE
	is.FLIndexCom(FLIndexCom())
	
	# but this is FALSE
	is.FLIndexCom(FLIndex())
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
