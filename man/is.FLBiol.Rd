% $Id: is.FLBiol.Rd,v 1.4.2.2 2005/12/19 11:13:48 iagoazti Exp $
\name{is.FLBiol}
\alias{is.FLBiol}
\title{FLBiol}
\description{Testing of FLBiol objects}
\usage{
  is.FLBiol(x)
}
\arguments{
  \item{x}          {object to test.}
}
\details{
}
\value{
	\code{is.FLBiol} returns \code{TRUE} or \code{FALSE}
}
\seealso{
    \code{\link{FLBiol-class}}
}
\examples{\dontrun{
    ple4.popln<-FLBiol(data(ple4))
    is.FLBiol(ple4.popln)
}
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
