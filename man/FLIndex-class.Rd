\name{FLIndex-class}
\docType{class}
\alias{FLIndex-class}
\alias{FLIndexCom-class}
\alias{FLIndexSurvey-class}
\alias{FLIndexAcoustic-class}
\alias{FLIndex}
\alias{FLIndexCom}
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
\alias{catch,FLIndexCom-method}
\alias{catch<-,FLIndexCom,FLQuant-method}
\alias{catch.wt,FLIndexCom-method}
\alias{catch.wt<-,FLIndexCom,FLQuant-method}
\alias{effort,FLIndexCom-method}
\alias{effort<-,FLIndexCom,FLQuant-method}
\alias{sel.pattern,FLIndexCom-method}
\alias{sel.pattern<-,FLIndexCom,FLQuant-method}
\alias{index.q,FLIndexCom-method}
\alias{index.q<-,FLIndexCom,FLQuant-method}
\alias{catch.n,FLIndexSurvey-method}
\alias{catch.n<-,FLIndexSurvey,FLQuant-method}
\alias{catch.wt,FLIndexSurvey-method}
\alias{catch.wt<-,FLIndexSurvey,FLQuant-method}
\alias{effort,FLIndexSurvey-method}
\alias{effort<-,FLIndexSurvey,FLQuant-method}
\alias{index.q,FLIndexSurvey-method}
\alias{index.q<-,FLIndexSurvey,FLQuant-method}
\alias{index.q,FLIndexAcoustic-method}
\alias{index.q<-,FLIndexAcoustic,FLQuant-method}

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

	By using the \code{class} argument, objects can be created of the derived FLIndex classes. This can
	also be achieved by using the corresponding creator functions: \code{FLIndexCom()},
	\code{FLIndexSurvey()} and \code{FLIndexAcoustic()}.
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
	The \code{FLIndexCom} class, designed for indices based on commercial catch data, adds the
		following slots:
	\describe{
		\item{\code{catch}:}{An \code{"FLQuant"} for the catch}
		\item{\code{catch.wt}:}{An \code{"FLQuant"} for the weight-at-age (or length) in the catch}
	    \item{\code{effort}:}{An \code{"FLQuant"} for the effort time series. The \code{units} slot should
			indicate the units used to measure it (days, hooks per line,...)}
    	\item{\code{sel.pattern}:}{An \code{"FLQuant"} for the selection pattern.}
    	\item{\code{index.q}:}{An \code{"FLQuant"} for the index catchability.}
	}
	The \code{FLIndexSurvey} class, designed for indices based on catch surveys, adds the
		following slots:
	\describe{
		\item{\code{catch.n}:}{An \code{"FLQuant"} for the catch by age or quant}
		\item{\code{catch.wt}:}{An \code{"FLQuant"} for the weight-at-age (or length) in the catch}
	    \item{\code{effort}:}{An \code{"FLQuant"} for the effort time series. The \code{units} slot should
			indicate the units used to measure it (days, hooks per line,...)}
    	\item{\code{index.q}:}{An \code{"FLQuant"} for the index catchability.}
	}
	The \code{FLIndexAcoustic} class, designed for indices based on acoustic surveys, adds the
		following slots to be basic class:
	\describe{
    	\item{\code{index.q}:}{An \code{"FLQuant"} for the index catchability.}
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
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLIndices-class}}
}
\examples{
fi <- FLIndex(name = 'index', desc = 'An index', index=FLQuant(dim=c(5,10,1,1,1)))
summary(fi)
fi <- FLIndex(name = 'index', desc = 'An index', index=FLQuant(dim=c(5,10,1,1,1)), class="FLIndexCom")
summary(fi)
}
\keyword{classes}
\keyword{methods}
