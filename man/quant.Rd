% $Id: quant.Rd,v 1.1.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{quant}
\docType{methods}
\alias{quant}
\alias{quant<-}
\alias{quant-methods}
\alias{"quant<-"-methods}
\title{Quant, first dimension of FLQuant}
\description{
	These two functions extract and modify the name of the first dimension of an FLQuant, the quant. These are generic functions, and can be overloaded for other classes.
}
\section{Methods}{
\describe{
    To obtain the present quant:
	\item{quant()}{Extracts the name of the first dimension of an FLQuant, the quant, as a character
	string.}

    To set or modify the quant:
    \item{quant<-}{Modifies the name of the first dimensions of an FLQuant.}
}}
\details{
	An FLQuant is a five dimensional array with fixed names for the last four dimensions (See
	\code{\link{FLQuant-class}}). The first dimension data is structured along can be different to
	accomodate different kinds of data, e.g. catches might be structured by age or length.

	The name of the first dimension can be set when a new object is created by using \code{\link{FLQuant}}
	through the quant input argument.

	Functions or methods requering data to be structured in a particular way should check for the quant
	of an FLQuant by using \code{quant()}.
}
\author{FLR Team}
\seealso{\code{\link{FLQuant-class}}}
\examples{
	flq <- FLQuant(rnorm(20), dim=c(6,10,1,1,1), quant='age')
	quant(flq)
	quant(flq) <- 'length'
}
\keyword{methods}
