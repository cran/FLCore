\name{FLBiol-class}
\docType{class}
\alias{FLBiol-class}
\alias{FLBiol}
\alias{apply,FLBiol,list,function-method}
\alias{setunits,FLBiol-method}
\alias{summary,FLBiol-method}
\alias{plot,FLBiol,missing-method}
\alias{transform,FLBiol-method}
\alias{name,FLBiol-method}
\alias{name<-,FLBiol,character-method}
\alias{desc,FLBiol-method}
\alias{desc<-,FLBiol,character-method}
\alias{n,FLBiol-method}
\alias{n<-,FLBiol,FLQuant-method}
\alias{m,FLBiol-method}
\alias{m<-,FLBiol,FLQuant-method}
\alias{wt,FLBiol-method}
\alias{wt<-,FLBiol,FLQuant-method}
\alias{fec,FLBiol-method}
\alias{fec<-,FLBiol,FLQuant-method}
\alias{spwn,FLBiol-method}
\alias{spwn<-,FLBiol,FLQuant-method}
\title{Class FLBiol}
\description{ A class to represent a biological population }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLBiol", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} }
    \item{\code{desc}:}{Object of class \code{"character"} }
    \item{\code{range}:}{Object of class \code{"numeric"} }
    \item{\code{n}:}{Object of class \code{"FLQuant"} }
    \item{\code{m}:}{Object of class \code{"FLQuant"} }
    \item{\code{wt}:}{Object of class \code{"FLQuant"} }
    \item{\code{fec}:}{Object of class \code{"FLQuant"} }
    \item{\code{spwn}:}{Object of class \code{"FLQuant"} }
  }
}
\section{Methods}{
  \describe{
    \item{as.FLBiol}{\code{signature(object = "FLBiol")}: Why should you code a \code{as} function for a class that you want to coerce to ?}
    \item{is.FLBiol}{\code{signature(object = "FLBiol")}: Check if an object is of class \code{FLBiol}.}
    \item{apply}{\code{signature(FLBiol,list,function)}: Usual apply function.}
    \item{plot}{\code{signature(FLBiol,list,function)}: Usual plot function.}
    \item{setunits}{\code{signature(FLBiol)}: Set units of your objects.}
    \item{summary}{\code{signature(FLBiol)}: Summary of the object.}
    \item{transform}{\code{signature(FLBiol)}: Transform of the object.}
    \item{window}{\code{signature(FLBiol)}: Transform of the object.}
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLBiols-class}}
}
\examples{
}
\keyword{classes}
