\name{as.FLIndex-methods}
\docType{methods}
\alias{as.FLIndex}
\alias{as.FLIndex-methods}
\alias{as.FLIndex,FLFleet-method}
\title{Method for creating an FLIndex object from other classes}
\description{
	Objects of class \code{\link{FLIndex-class}} can be created by coercing objects of oter classes.
	A method exists for extracting catch and effort information from an \code{\link{FLFleet-class}}
	object. This is a generic method, so implementations for other classes can be written.
}
\section{Methods}{
	\describe{
		\item{siganture(FLFleet)}{Create an FLIndex object from catch and effort data in an FLFleet.}
	}
}
\keyword{methods}
