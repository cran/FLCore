% $Id: is.FLStocks.Rd,v 1.1.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{is.FLStocks}
\alias{is.FLStocks}
\title{FLStocks}
\description{Testing for FLStocks list objects}
\usage{
  is.FLStocks(x)
}
\arguments{
  \item{x}   {a \code{FLStocks} object.}
}
\value{
	\code{is.FLStocks} returns \code{TRUE} or \code{FALSE} depending on whether the object is or not of class \code{FLStocks}.
}
\seealso{
    \code{\link{FLStocks-class}}
}
\examples{\dontrun{
    data(ple4)
    fl <- FLStocks()
    fl[[1]] <- ple4
    is.FLStocks(fl)
}
}
\author{FLR Team}
\keyword{classes}
\keyword{manip}
