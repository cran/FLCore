\name{FLQuants-class}
\docType{class}
\alias{FLQuants-class}
\alias{FLQuants}
\alias{as.data.frame,FLQuants,ANY,missing-method}
\alias{summary,FLQuants-method}
\title{Class "FLQuants"}
\description{
This class encapsulates a list of \code{"FLQuant"} objects.}

\section{Creating Objects from the Class}{
Objects of this classe should be created with a call to the contructor function \code{FLQuants(...)}, where \code{...} can be a list of \code{"FLQuant"} objects of several \code{"FLQuant"}.

Another way to create \code{"FLQuants"} is with the function \code{mcf} of \code{FLDA} which guarantees that all \code{"FLQuant"} objects have the same dimensions and dimnames. 
}

\section{Slots}{
No slots ! See details below.
}

\section{Extends}{
Class \code{"list"}, directly.

}

\section{Methods}{
Inherits from \code{"list"}.

\describe{
	\item{xyplot:}{
		Lattice method to plot several FLQuants it works by adding datasets on the right side of the formula, so plotting them over each other.
	}
    \item{as.data.frame}{Coerces \code{"FLQuants"} into \code{"data.frame"}.}
    \item{summary}{Summarized information on the object.}
}
}

\section{Details}{
This class, together with other \code{FLR} classes which are named with the plural of some \code{FLR} class (e.g. \code{"FLStocks"}), are lists of those classes.

The idea was to provide a mechanism to hold several complex classes that could be used for exploratory analysis like setting a VPA model, or testing HCR procedures, etc.

The result is not satisfactory as these classes extend \code{"list"} and mix to programming paradigms quite different \code{S3} and \code{S4}. We are still researching how to develop a better mechanism butfor now that's what we have to offer. So proceed with care and expect some changes in the future to this mechanism.
}


\seealso{
  \code{\link{FLQuant-class}}, \code{\link{xyplot,formula,FLQuants-method}}
}
\author{
	The FLCore Team	
	}
\keyword{classes}
