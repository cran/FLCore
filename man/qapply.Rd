% $Id: qapply.Rd,v 1.1.2.1 2007/01/18 17:20:16 imosqueira Exp $
\name{qapply}
\alias{qapply}
\title{Apply a Function over the FLQuant slots of an FLR S4 object}
\description{
	\code{qapply} returns either a list or an object of the same class as X. Each element of which is the result of applying FUN to the corresponding FLQuant slot of X. If the result of applying FUN over an FLQuant is an object of class FLQuant, \code{qapply} will return an object of the same class as X where each FLQuant has been substituted with the object returned by FUN.

	When the result of FUN is of any other class, \code{qapply} will return a named list with the same names as those of the FLQuant slots in the input object.

	This function is a new generic in FLCore, and methods are available for the various FLCore classes.
}
\usage{qapply(X, FUN, ...)}
\arguments{
  \item{X}{An FLR S4 object}
  \item{FUN}{the function to be applied to each element of X: see Details.}
  \item{...}{optional arguments to FUN.}
}
\details{
	FUN is found by a call to match.fun and typically is either a function or a symbol (e.g. a backquoted name) or a character string specifying a function to be searched for from the environment of the call to qapply.

	Function FUN must be able to accept as input any each of the FLQuant slots of X.

	If X has length 0, the return value of sapply is always a 0-length list. 
}
\seealso{
	\code{\link{lapply}}, \code{\link{apply}}
}
\author{FLR Team. I. Mosqueira}
\examples{
fli <- FLIndex(index=FLQuant(rnorm(30), dim=c(3,10)))
summary(qapply(fli, window, start=3, end=5))
is.list(qapply(fli, quant))
}
\keyword{methods}
