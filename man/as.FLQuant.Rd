% $Id: as.FLQuant.Rd,v 1.7.2.2 2005/12/20 08:16:00 iagoazti Exp $
\name{as.FLQuant}
\alias{is.FLQuant}
\alias{as.FLQuant}
\alias{as.FLQuant-methods}
\alias{as.FLQuant,data.frame-method}
\alias{as.FLQuant,vector-method}
\alias{as.FLQuant,matrix-method}
\alias{as.FLQuant,array-method}
\alias{as.FLQuant,FLQuant-method}

\title{as.FLQuant}

\description{Creates from other objects or tests for an FLQuant}

\usage{
   as.FLQuant(x, ...)
   is.FLQuant(x)
}
\arguments{
    \item{x}{For \code{as.FLQuant} matrix, numeric vector, array, data.frame or FLQuant.
        For \code{is.FLQuant}, any R object.}
	\item{...}{Extra arguments, including dim, dimnames, quant, and units. Deafults to 
	dimnames = "missing", units = "NA", quant = NULL}
}

\details{

    \code{as.FLQuant} can be used to alter the dimnames and/or units of an FLQuant,
    although other methods exist for this. If \code{x} is a matrix, it is considered
    to be a year-by-age matrix, and all other dimensions will be considered singular.
    Ages and years are taken from the matrix row and columns names, respectively, if
    those exist.

    To coerce a numeric vector, \code{Dimnames} needs to be specified, in order to
    indicate along which dimension does the vector run.

    When \code{x} is a data.frame, columns should have names corresponding to the FLQuant
    dimensions, while the last column (usually named \code{Freq}) holds the data.

    Finally, if \code{x} is an array, its dimensions should not exceeded those of an
    FLQuant (five). If the array dimensions are less than five, all other dimensions will
    be considered unique. Dimnames of the array must also match those of an FLQuant with
    the same dimensions.
}
\value{
    \code{as.FLQuant} returns an \code{FLQuant} object with original data converted.
    \code{is.FLQuant} returns 'TRUE' if it is an \code{FLQuant} object, 'FALSE' otherwise.
}
\author{FLR Team}
\seealso{
    \code{\link{FLQuant-class}},
    \code{\link{units}}
}
\examples{
# Generate an array containing random numbers, and convert it into an FLQuant
sq <- FLQuant(array(rnorm(120, mean=2), dim=c(20,3,2,1,1),
      dimnames=list(age=as.character(1:20), 
                    year=c("1999", "2000", "2001"), 
                    unit=c("male", "female"), 
                    season="all", area="all")), units="Tons")
summary(sq)

# as.FLQuant
arr <- array(rnorm(24), dim=c(3, 4, 2))
class(arr)
flq <- as.FLQuant(arr, units='kg')
summary(flq)
class(flq)

is.FLQuant(flq)        # Yes
}
\keyword{classes}
\keyword{manip}
