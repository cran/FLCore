% $Id: units.Rd,v 1.3.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{units}
\docType{methods}
\alias{units}
\alias{units<-}
\alias{units-methods}
\alias{"units<-"-methods}
\alias{units,FLQuant-method}
\alias{units<-,FLQuant-method}

\title{Extract and modify the units slot of an object}

\description{
    This is a generic method defined at least for the \code{\link{FLQuant-class}},
    but can be extended to other classes.

    An FLQuant object consist of a five-dimensional array plus a character slot
    called \code{units}, where the units of measurement of the data can be stored.
    Any character string is allowed, and at present methods and functions do not 
    make use of its content, although this might change in the future. The intention
    is to facilitate accurate storage of data and output.
}

\section{Methods}{
	\describe{
		\item{signature(FLQuant)}{An FLQuant}
	}
}
\details{
    To obtain the present value of the \code{units} slot:
    \item{units}{\code{signature("FLQuant")}}

    To set the content of the \code{units} slot:
    \item{units<-}{\code{signature("FLQuant")}}
}
\author{FLR Team}

\seealso{
 \code{\link{FLQuant-class}}
}

\examples{
    flq <- FLQuant()
    units(flq)
    units(flq) <- "kg"
}
\keyword{methods}
