% $Id: setPlusGroup.Rd,v 1.3.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{setPlusGroup}
\alias{setPlusGroup}
\title{setPlusgroup for FLStock}
\description{
    Changes the plus group of an FLStock object, adding up all age-structured
    quantities for age equal or older to the given value.
}
\usage{
setPlusGroup(stock, plusgroup = "missing")
}
\arguments{
  \item{stock}{An FLStock object}
  \item{plusgroup}{Age to be used as start of plus group}
}
\value{
    An FLStock
}
\author{FLR Team}
\seealso{\code{\link{FLStock}}}
\examples{
}
\keyword{manip}
