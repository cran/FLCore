% $Id: FLQuant.Rd,v 1.9.2.2 2005/12/20 08:16:00 iagoazti Exp $
\name{FLQuant}
\alias{FLQuant}
\alias{FLQuant-methods}
\alias{FLQuant,missing-method}
\alias{FLQuant,vector-method}
\alias{FLQuant,matrix-method}
\alias{FLQuant,array-method}
\alias{FLQuant,FLQuant-method}

\title{FLQuant}

\description{Creates a new FLQuant}

\usage{
   FLQuant(object, dimnames = "missing", dim = "missing", quant = NULL, units = "NA")
}
\arguments{
    \item{object}{A vector, matrix, array or FLQuant to be used when creating the FLQuant.}
	\item{dimnames}{A list specifying the dimnames of the FLQuant.}
    \item{dim}{Dimensions of the FLQuant, if object is not a 5 dimensional array or FLQuant.}
	\item{quant}{The name of the first dimension. A character string. Defaults to "quant".}
    \item{units}{A character with the measurement units of the data. Defaults to "NA"
        (Don't mistake with \code{\link{NA}}).}
}

\details{

	\code{FLQuant()} is the basic creator function for object of class FLQuant. The
	function accepts objects of various classes as input, and its behaviour adapts to
	them, as well as to the values specified for \code{dim} and \code{dimnames}

	\item{missing}{If no input object is given, an empty FLQuant is returned, with dimensions
	set by \code{dim} and dimension names equal to \code{dimnames}, if given.}

	\item{vector}{Vectors, including those of length one, are assumed to represent time
	series along the \code{year} dimension. If \code{dim} or \code{dimnames} are specified,
	they are used to set the final dimensions of the FLQuant object. If the second dimension
	given differs from the length of the input vector, the later is reused. If both \code{dim}
	and \code{dimnames} are given, their dimensions must match.}
	
	\item{matrix or array}{Matrices in R are simply 2-D arrays. If the input array has less
	than five dimensions, it is assumed the missing ones are of length one.}

	The name of the first dimension can be specified using the \code{quant} input variable. 
	But if \code{dimnames} are specified, the quant name is taken from the first item on the
	dimnames list.
}
\value{
    \code{FLQuant} returns an \code{FLQuant} object with the specified data, dimensions,
	dimension names, quant and units.
}
\author{FLR Team}
\seealso{
    \code{\link{FLQuant-class}},
    \code{\link{units}},
    \code{\link{quant}}
}
\examples{
# Generate an array containing random numbers, and convert it into an FLQuant
sq <- FLQuant(array(rnorm(120, mean=2), dim=c(20,3,2,1,1),
      dimnames=list(age=as.character(1:20), 
                    year=c("1999", "2000", "2001"), 
                    unit=c("male", "female"), 
                    season="all", area="all")), units="Tons")
}
\keyword{classes}
\keyword{manip}
