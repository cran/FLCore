\name{FLBiols-class}
\docType{class}
\alias{FLBiols}
\alias{FLBiols-class}
\alias{show,FLBiols-method}
\alias{summary,FLBiols-method}
\title{Class FLBiols}
\description{An FLBiols object is a collection of different FLBiol datasets.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLBiols", ...) or directly with \code{FLBiols()}}
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. Holds a variable number of \code{FLBiol} objects}
    \item{\code{desc}:}{A description of the list.}
  }
}
\section{Extends}{
Class \code{"list"}, from data part.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(FLBiols)}: Summary of the object.}
    \item{show}{\code{signature(FLBiols)}: Shows the object.}
    \item{is.FLBiols}{\code{signature(object = "FLBiols")}: Check if an object is of class \code{FLBiols}.}
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLBiol-class}}
}
\keyword{classes}
