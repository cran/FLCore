\name{FLFleet-class}
\docType{class}
\alias{FLFleet}
\alias{FLFleet-class}
\alias{summary,FLFleet-method}
\alias{plot,FLFleet,missing-method}
\alias{transform,FLFleet-method}
\alias{revenue,FLFleet-method}
\alias{name,FLFleet-method}
\alias{name<-,FLFleet,character-method}
\alias{desc,FLFleet-method}
\alias{desc<-,FLFleet,character-method}
\alias{effort,FLFleet-method}
\alias{effort<-,FLFleet,FLQuant-method}

\title{FLFleet class and methods}

\description{
 \code{FLFleet} creates the FLFleet objects. These objects contain fleet data.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLFleet", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{the name of the fleet.}
    \item{\code{desc}:}{a description of the fleet.}
    \item{\code{range}:}{age and year ranges.}
    \item{\code{catches}:}{catches by species as a list of \code{FLCatch} objects.}
    \item{\code{effort}:}{An \code{FLQuant} holding the fishing effort}
    \item{\code{capacity}:}{An \code{FLQuant} holding the capacity of the fishing fleet}
    \item{\code{vcost}:}{variable  costs.}
    \item{\code{fcost}:}{fixed costs.}
    \item{\code{crewshare}:}{crewshare.}
  }
}
\section{Methods}{
  \describe{

Summarise contents:
    \item{summary}{\code{signature(obj = "FLFleet")}: prints the dimensions for each slot
    in the object, including the dimensions of FLCatch objects and FLQuants in effort and capacity.}

Plot contents:
    \item{plot}{\code{signature(x = "FLFleet", y = "missing")}: Generates a number of
    standard plots for an \code{FLFleet}. Currently, only one plot style can be specified, by
    using the \code{type} parameter. If type="summary", a four-section plot is
    created showing the effort and capacity of the fleet, a sum of the three types of costs in the 
    object and the total landings of each of the catches of the Fleet. This "summary" type is also the default.}


Object coercion: 
    \item{as.FLStock}{\code{signature(obj = "FLFleet")}: Creates a new FLStock based upon the input FLFleet. If data for more than one stock are present inside the \code{FLFleet} object, one can be selected using \code{name} and \code{gear}. If \code{name} or \code{gear} is not specified, and data for only one stock is present, the new \code{FLStock} is filled with the corresponding data. When the fleet catches more than one stock, it is necessary to specify one of them. \code{name} and/or \code{gear} then identifies the stock inside the \code{FLFleet} object to be used by comparing to \code{name} and \code{gear} slot of each \code{FLCatch}. If more than one \code{FlCatch} matches name, the data in the slots is summed for numbers, catch, discards and landings. cach.wt, landings.wt and discards.wt are average, weighted by numbers in the individual catches.}

Indexing and subsetting:
    \item{\link{window}:}{ To specify a temporal window, along the year dimension,
		over which to subset or extend the object.}

Revenue calculation:   
    \item{revenue}{\code{signature(obj = "FLFleet")}: Calculates the revenue of the fleet. By default, the gross revenue is calculated by the sum of the products of price, landings.n and landings.wt of all FLCatches. By specifying type="nett", the nett revenue is calculated by subtracting the sum of crewshare, vcost and fcost from the gross revenue.}  

Other:
\item{transform}{\code{signature(obj = "FLFleet")}: Alters the content of individual slots in
	a FLFleet object. Only the data is altered, not dimnames.}

  }
}
\references{}
\author{FLR Team}

\seealso{
   \code{\link{FLCatch-class}}
   \code{\link{is.FLFleet}},
   \code{\link{as.FLFleet}},
}
\examples{   

# Create a new fleet
catch <- FLQuant(NA, dimnames=list(age=as.character(1:12), year=as.character(c(1991:2000)),
          unit="all", season="all", area="all"))
fleet <- FLQuant(NA, dimnames=list(quant="all", year=as.character(c(1991:2000)),
          unit="all", season="all", area="all"))
my.fleet <- FLFleet(iniFLQuantFleet=fleet, iniFLQuantCatch=catch, catchname="examplecatch",
desc="Just an examplefleet")
my.effort <- FLQuant(matrix(rpois(20,120), nrow=1))
my.range <- c(1, 8, 8, 1980, 2000)
my.fleet@effort <- my.effort
my.fleet@catches[[1]]@range <- my.range
summary(my.fleet)
}
\keyword{classes}

