% $Id: FLSR.Rd,v 1.5.2.2 2006/08/04 09:37:14 dorleta Exp $
\name{FLSR}
\alias{FLSR}
\alias{FLSR-methods}


\title{FLSR}

\description{Creates a new FLSR object.}

\usage{
   FLSR(name = character(0), desc = character(0), model = character(0),
    quant = "quant", ...)
}
\arguments{
    \item{name}{A character string with the name of the stock.}
	\item{desc}{A character string to describe the data stored in the object or other information of interest.}
    \item{model}{Name of the stock-recruitment relationship used in the model.}
	\item{quant}{The name of the first dimension. A character string. Defaults to "quant".}
    \item{...}{Other slots of the 'FLSR' class.}
}

\details{

    If no 'FLQuant' slots are supplied in the call to the constructor 'FLSR()',
an 'FLSR' object with dimension '(1,1,1,1,1)' in the 'FLQuant' slots is returned.
Otherwise if one of the arguments in the call to the function is an 'FLQuant' slot,
an 'FLSR' object which 'FLQuant' slots are of the same dimension of the input
'FLQuant' is returned. If two ore more 'FLQuant' slots are supplied in the call
to the function, they must have the same dimension. The slots related to the model
fit (i.e. 'params', 'se', 'covar',...) can be supplied in the call to the constructor as well.
}

\value{
    \code{FLSR} returns an object of class 'FLSR'.
}
\author{FLR Team}
\seealso{
    \code{\link{FLSR-class}},

}
\examples{

    flsr <- FLSR()
    flsr <- FLSR(name = "Hake", desc = "An empty 'FLSR' object")

    data(ple4)
    ssb <- ssb(ple4)
    rec <- stock.n(ple4)[1,]

    flsr <- FLSR(name = "Plaice", desc = "Plaice in area IV, data obtained from 'ple4' 'FLSTock' object",
        ssb = ssb)
    flsr <- FLSR(name = "Plaice", desc = "Plaice in area IV, data obtained from 'ple4' 'FLSTock' object",
        ssb = ssb, rec = rec)
    flsr <- FLSR(name = "Plaice", desc = "Plaice in area IV, data obtained from 'ple4' 'FLSTock' object",
        model = "ricker", params = matrix(c(3.7, 3.3e-6,NA),1))
}
\keyword{classes}
\keyword{methods}
