% $Id: FLStocks-class.Rd,v 1.1.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{FLStocks-class}
\docType{class}
\alias{FLStocks}
\alias{FLStocks-class}
\alias{summary,FLStocks-method}
\title{Class FLStocks}
\description{An FLStocks object is a collection of different FLStock datasets.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLStocks", ...) or directly with \code{FLStocks()}}
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. Holds a variable number of \code{FLStock} objects}
    \item{\code{desc}:}{A description of the list.}
  }
}
\section{Extends}{
Class \code{"list"}, from data part.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "FLStocks")}: Print concise summary of the contents of the object}
  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLStock-class}}
}
\examples{
    # Create a new empty stock, giving a name and desc.
    my.ple <- FLStock(name = "plaice", desc = "Just an example...")
    summary(my.ple)
    
    # Create an FLStock with the same dimensions as the FLQuant object catch.
    catch <- FLQuant(array(round(rnorm(120, 5000,500)), dim=c(12,10,1,1,1),
              dimnames=list(age=as.character(1:12), year=as.character(c(1991:2000)),
              unit="all", season="all", area="all")))
    my.stock <- FLStock(name = "My Stock", desc = "Just an example...", iniFLQuant=catch)
    my.stock@catch <- catch
    
    # Colletc both stocks together.
    my.stocks <- FLStocks(my.ple, my.stock)
}
\keyword{classes}
