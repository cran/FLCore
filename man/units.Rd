% $Id: units.Rd,v 1.3.2.8 2006/03/28 15:38:22 iagoazti Exp $
\name{units}
\docType{methods}
\alias{units}
\alias{units<-}
\alias{units-methods}
\alias{"units<-"-methods}
\alias{units,FLQuant-method}
\alias{units<-,FLQuant,character-method}
\alias{units,FLStock-method}
\alias{units<-,FLStock,list-method}

\title{Extract and modify the units slot of an object}

\description{
    This is a generic method defined at least for the \code{\link{FLQuant-class}},
    but has been extended to other classes (see examples).

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

    data(ple4)
    units(ple4)
    units(ple4) <- list(harvest="f")

    my.FLStock <- FLStock()
    units(my.FLStock) <- units(ple4)


}
\keyword{methods}
