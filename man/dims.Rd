\name{dims}
\alias{dims}
\alias{dims-methods}
\alias{dims,FLQuant-method}
\alias{dims,FLIndex-method}
\title{Provide information onn the dimensions and range of an object}
\description{
	A descriptive list of the dimensions and dimension names, as well as ranges along different
	dimensions for different FLCore objects. Information is taken from range as well as directly
	from the objects at call time.
}
\usage{
dims(obj, ...)
}
\arguments{
  \item{obj}{Any object.}
  \item{\dots}{Any extra arguments available for some classes.}
}
\details{
	Return object is a list
  ~~ If necessary, more details than the description above ~~
}
\value{
	Return object is a nmed list containing a number of slots, depending on the object class. For
	example, for \code{\link{FLQuant-class}}, \code{dims} returns:
  \item{quant}{Length of the first (quant) dimension. The name of this slot reflects the quant name.}
  \item{min}{Starting value of the quant dimension, if numeric.}
  \item{max}{Final value of the quant dimension, if numeric.}
  \item{year}{Length of the year dimension.}
  \item{minyear}{First year.}
  \item{maxyear}{Last year.}
  \item{unit}{Number of units.}
  \item{season}{Number of seasons.}
  \item{area}{Number of areas.}
}
\author{FLR Team}
\seealso{\code{\link{dim}}, \code{\link{dimnames}}}
\examples{
	dims(FLQuant(rnorm(10), dim=c(2,5,1,1,1), quant='age'))
}
\keyword{methods}
