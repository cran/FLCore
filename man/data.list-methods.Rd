\name{data.list-methods}
\docType{methods}
\alias{data.list-methods}
\alias{data.list}
\alias{data.list,FLQuants-method}
\alias{data.list,list-method}
\title{Creates data.frame with list of values}

\description{
This method gets a list of \code{"FLQuant"} and coarces it into a data.frame with a list of the values of each \code{"FLQuant"}. It is very similar to \code{"as.data.frame"} but instead of providing each \code{"FLQuant"} in a new column, all objects are "rbinded". It's intended to be helpfull for ploting with \code{xyplot}.
}

\section{Generic Function}{
\describe{
\item{Usage}{data.list(object, \dots)}
\item{Arguments:}{\ }
\describe{
	\item{\ }{\code{object}: list of \code{"FLQuant"} objects}
	\item{\ }{\code{\dots}: additional, optional arguments if requested}
}
\item{Value}{\code{"FLQuants"} object}
}}

\section{Methods}{
\describe{
\item{object="list"}{creates a \code{"data.frame"} object from a list of \code{"FLQuant"} objects rbinding each object in the list. A new column is added with the name of the object \code{"qname"}}
}
}

\seealso{
  \code{\link[FLCore]{FLQuant-class}}, \code{\link[FLCore]{FLQuants-class}}
}
\examples{
data(ple4)
flqs <- FLQuants(list(catch.n(ple4), landings.n(ple4), discards.n(ple4)))
data.list(flqs)
}
\keyword{methods}
