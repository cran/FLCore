\name{FLIndex-class}
\docType{class}
\alias{FLIndex-class}
\alias{FLIndexSurvey-class}
\alias{FLIndexAcoustic-class}
\alias{FLIndex}
\alias{FLIndexSurvey}
\alias{FLIndexAcoustic}
\alias{apply,FLIndex,list,function-method}
\alias{plot,FLIndex,missing-method}
\alias{summary,FLIndex-method}
\alias{transform,FLIndex-method}
\alias{name,FLIndex-method}
\alias{name<-,FLIndex,character-method}
\alias{desc,FLIndex-method}
\alias{desc<-,FLIndex,character-method}
\alias{distribution,FLIndex-method}
\alias{distribution<-,FLIndex,character-method}
\alias{index,FLIndex-method}
\alias{index<-,FLIndex,FLQuant-method}
\alias{index.var,FLIndex-method}
\alias{index.var<-,FLIndex,FLQuant-method}
\alias{catch.n,FLIndex-method}
\alias{catch.n<-,FLIndex,FLQuant-method}
\alias{catch.wt,FLIndex-method}
\alias{catch.wt<-,FLIndex,FLQuant-method}
\alias{effort,FLIndex-method}
\alias{effort<-,FLIndex,FLQuant-method}
\alias{index.q,FLIndex-method}
\alias{index.q<-,FLIndex,FLQuant-method}
\alias{sel.pattern,FLIndex-method}
\alias{sel.pattern<-,FLIndex,FLQuant-method}
\alias{trim,FLIndex-method}

\title{Class FLIndex}
\description{A class designed to hold data and parameters related to abundance indices}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLIndex", ...)}, through the creator function \code{FLIndex()}.
}
\section{Creator Function}{
    The function \code{FLIndex()} creates a new \code{FLIndex} object. To initialise the object,
	an FLQuant should be provided for the index slot. An empty FLQuant can be used to create
	an empty FLIndex of the right dimensions, as in

	\code{FLIndex(index=FLQuant(dim=c(5,10,1,1,1)))}
}
\section{Slots}{
	Objects of the FLIndex class contain the following slots:
	\describe{
    	\item{\code{name}:}{The name of the index or fleet}
	    \item{\code{desc}:}{Description of the index}
    	\item{\code{range}:}{Year and age range, and specified plus group age. A vector of length 5.}
	    \item{\code{distribution}:}{A character string for the probability density function assumed
			for the error estructure}
	    \item{\code{index}:}{An \code{"FLQuant"}  for the index (CPUE or other) in numbers, either quant 
		(i.e., age) structured, or not.}
	    \item{\code{index.var}:}{An \code{"FLQuant"} for the index variance}
	}
}
\section{Methods}{
  \describe{
    \item{apply}{\code{signature(X = "FLIndex", MARGIN = "list", FUN = "function")}: ... }
    \item{dims}{\code{signature(obj = "FLIndex")}: ... }
    \item{plot}{\code{signature(x = "FLIndex", y = "missing")}: ... }
    \item{summary}{\code{signature(object = "FLIndex")}: ... }
    \item{transform}{\code{signature(x = "FLIndex")}: ... }
    \item{window}{\code{signature(x = "FLIndex")}: ... }
    \item{trim}{\code{signature(x = "FLIndex")}: ... }
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLIndices-class}}
}
\examples{
fi <- FLIndex(name = 'index', desc = 'An index', index=FLQuant(dim=c(5,10,1,1,1)))
summary(fi)
}
\keyword{classes}
\keyword{methods}
