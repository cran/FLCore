% $Id: quantOps.Rd,v 1.1.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{quant}
\alias{quantSums}
\alias{quantMeans}
\alias{yearSums}
\alias{yearMeans}
\alias{unitSums}
\alias{unitMeans}
\alias{seasonSums}
\alias{seasonMeans}
\alias{areaSums}
\alias{areaMeans}
\alias{yearTotals}
\alias{quantTotals}
\title{Common summary operations for FLQuants}
\description{
	Sums and means of FLQuants along any of the five dimensions can be obtained by using these set of
	functions.
}
\usage{
quantSums(x, na.rm=TRUE)
quantMeans(x, na.rm=TRUE)
yearSums(x, na.rm=TRUE)
yearMeans(x, na.rm=TRUE)
unitSums(x, na.rm=TRUE)
unitMeans(x, na.rm=TRUE)
seasonSums(x, na.rm=TRUE)
seasonMeans(x, na.rm=TRUE)
areaSums(x, na.rm=TRUE)
areaMeans(x, na.rm=TRUE)
quantTotals(x)
yearTotals(x)
}
\arguments{
  \item{x}{An FLQuant object.}
  \item{na.rm}{Ignores or not NAs.}
}
\details{
}
\value{
	All these functions return an FLQuant with the relevant dimension reduced to length 1, containing
	the means or sums along the selected dimension. names on the chosen dimension are also modified.

	The exceptions are \code{quantTotals} and \code{yearTotals}, which return an object of the same
	dimensions as the original containing the sums along the quant or year dimensions respectively. This
	is useful for calculating proportions, for example.
}
\author{FLR Team}
\seealso{\code{\link{FLQuant-class}}}
\examples{
}
\keyword{manip}
