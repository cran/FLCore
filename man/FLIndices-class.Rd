\name{FLIndices-class}
\docType{class}
\alias{FLIndices-class}
\alias{FLIndices}
\alias{summary,FLIndices-method}
\alias{plot,FLIndices,missing-method}
\alias{trim,FLIndices-method}
\alias{[,FLIndices-method}
\alias{lapply,FLIndices,function-method}

\title{Class FLIndices}
\description{
    A list of \code{FLIndex} objects}
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("FLIndices", ...)}, or through the 
    creator function \code{FLIndices()}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{The list}
    \item{\code{desc}:}{A description of the collection}
  }
}
\section{Extends}{
Class \code{"list"}.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "FLIndices")}: Outputs a summary of the content of the object.}
    \item{trim}{\code{signature(object = "FLIndices")}: Applies the \code{trim} method to all slots.}
    \item{[}{\code{signature(x = "FLIndices")}: Subsets an FLIndices object.}
    \item{lapply}{\code{signature(X = "FLIndices", FUN = "function")}: ... }
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLIndex-class}}
}
\examples{\dontrun{
# Create two FLIndex objects, and collect them together in an FLIndices collection
my.index1 <- FLIndex(name="index #1", method="Lognormal GLM")
my.index2 <- FLIndex(name="index #2", method="Poisson GLM")
my.indices <- FLIndices(my.index1, my.index2, desc="An example of an FLIndices object")
summary(my.indices)

# We do not need individual components any more
rm(my.index1, my.index2)

# To access one index, use:
my.indices[[2]]	# Get the second index dataset in the collection

is.FLIndices(my.indices)
}
}
\keyword{classes}
\keyword{methods}
