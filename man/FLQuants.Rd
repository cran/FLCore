\name{FLQuants-class}
\docType{class}
\alias{FLQuants-class}
\alias{FLQuants}
\title{FLQuants}
\description{
This class is a list of FLQuant objects.}
\section{Slots}{
	\describe{
    	\item{\code{.Data}:}{Object of class \code{"list"}}
  	}
}
\section{Methods}{
\describe{
	\item{xyplot:}{
		Lattice method to plot several FLQuants it works by adding datasets on the right side of the formula, so plotting them over each other.
	}
}
}
\section{Creating Objects from the Class}{
Objects of S4 classes can be created with a call to the \code{new} method, a call of the contructor function (usually with 
the same name has the class) or by coercing other class objects by calling some \code{as.*} function.  
\describe{
	\item{new:}{not advisable}
	\item{as:}{not available}
	\item{constructor function:}{FLQuants(FLQuant01, FLQuant02, desc="my set of FLQuants")}
}
}
\seealso{
  \code{\link{FLQuant-class}}, \code{\link{xyplot,FLQuants-method}}
}
\author{
	The FLCore Team	
	}
\keyword{classes}
