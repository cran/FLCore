\name{FLFleets-class}
\docType{class}
\alias{FLFleets}
\alias{FLFleets-class}
\alias{summary,FLFleets-method}
\alias{as.FLStock,FLFleets-method}

\title{Class FLFleets}
\description{FLFleets is a collection of different FLFleet objects}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLFleets", ...)}.
    ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{A \code{list} with \code{FLFleet} objects}
    \item{\code{desc}:}{A description of the object}
  }
}
\section{Extends}{
Class \code{"list"}.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "FLFleets")}: Prints a summary of the object contents}
    \item{as.FLStock}{\code{signature(obj = "FLFleets")}: Creates a new FLStock based upon the input FLFleet. \code{name} and \code{gear} identify the catches inside the \code{FLFleets} object to be used by comparing to \code{name} and \code{gear} slot of each \code{FLCatch}. If more than one \code{FlCatch} matches the stock specified, the the data are coerced by summing the numbers and catch, discrads and landings. landings.wt, catch.wt and discards.wt are averaged weighing by the numbers.}.

  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLFleet-class}}, \code{\link{is.FLFleets}}
}
\examples{
    # Create an empty FLFleet object from two empty FLFleet obejects.
   iniFLQuantCatch = FLQuant(160, dim = c(8,20,1,1,1))
   iniFLQuant = FLQuant(10, dim = c(1,20,1,1,1))
   my.fleets <-FLFleets() 
   my.fleets[[1]] <- FLFleet(iniFLQuantCatch=iniFLQuantCatch, iniFLQuant=iniFLQuant, name = "Otter trawl", desc = "Just an example...")
   my.fleets[[2]] <- FLFleet(iniFLQuantCatch=iniFLQuantCatch, iniFLQuant=iniFLQuant, name = "Trawl", desc = "Just an example...")

   is.FLFleets(my.fleets)
}
\keyword{classes}
\keyword{methods}
