\name{FLCatch-class}
\docType{class}
\alias{FLCatch-class}
\alias{FLCatch}
\alias{summary,FLCatch-method}
\alias{plot,FLCatch,missing-method}
\alias{transform,FLCatch-method}
\alias{computeCatch,FLCatch-method}
\alias{computeLandings,FLCatch-method}
\alias{computeDiscards,FLCatch-method}
\alias{name,FLCatch-method}
\alias{name<-,FLCatch,character-method}
\alias{gear,FLCatch-method}
\alias{gear<-,FLCatch,character-method}
\alias{range,FLCatch-method}
\alias{range<-,FLCatch,numeric-method}
\alias{catch,FLCatch-method}
\alias{catch<-,FLCatch,FLQuant-method}
\alias{catch.n,FLCatch-method}
\alias{catch.n<-,FLCatch,FLQuant-method}
\alias{catch.wt,FLCatch-method}
\alias{catch.wt<-,FLCatch,FLQuant-method}
\alias{catch.sel,FLCatch-method}
\alias{catch.sel<-,FLCatch,FLQuant-method}
\alias{landings,FLCatch-method}
\alias{landings<-,FLCatch,FLQuant-method}
\alias{landings.n,FLCatch-method}
\alias{landings.n<-,FLCatch,FLQuant-method}
\alias{landings.wt,FLCatch-method}
\alias{landings.wt<-,FLCatch,FLQuant-method}
\alias{landings.sel,FLCatch-method}
\alias{landings.sel<-,FLCatch,FLQuant-method}
\alias{discards,FLCatch-method}
\alias{discards<-,FLCatch,FLQuant-method}
\alias{discards.n,FLCatch-method}
\alias{discards.n<-,FLCatch,FLQuant-method}
\alias{discards.wt,FLCatch-method}
\alias{discards.wt<-,FLCatch,FLQuant-method}
\alias{discards.sel,FLCatch-method}
\alias{discards.sel<-,FLCatch,FLQuant-method}
\alias{catchability,FLCatch-method}
\alias{catchability<-,FLCatch,FLQuant-method}
\alias{price,FLCatch-method}
\alias{price<-,FLCatch,FLQuant-method}

\title{Class FLCatch}
\description{An internal class for FLFleet objects that stores stock or species specific information on landings, discards and catches}
\section{Objects from the Class}{
    Objects of this class are not meant to be created by the user, but only by those classes and functions that use it. This can be carried out by a call to \code{new("FLCatch")} or through the constructor function \code{FLCatch()}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Name of the stock or species}
    \item{\code{gear}:}{gear used for generating catches}
    \item{\code{range}:}{Age and year range of the catch data}
    \item{\code{catch}:}{An FLQuant holding the total catch (landings plus discards)
        in weight for all ages}
    \item{\code{catch.n}:}{Catch in numbers by age}
    \item{\code{catch.wt}:}{Weight at age in the catch, usually obtained from those for
        landings and discards (see \code{update})}
    \item{\code{catch.sel}:}{Selectivity by age of the catch}
    \item{\code{discards}:}{An FLQuant holding the total discards in weight for all ages}
    \item{\code{discards.n}:}{Discards by age in numbers}
    \item{\code{discards.wt}:}{Weight at age of the discards}
    \item{\code{discards.sel}:}{Selectivity by age of the discards}
    \item{\code{landings}:}{An FLQuant of the total landings for all ages}
    \item{\code{landings.n}:}{Landings in numbers by age}
    \item{\code{landings.wt}:}{Weight at age of landings}
    \item{\code{landings.sel}:}{Selectivity by age of the landings}
    \item{\code{catchability}:}{catchability coefficient}
    \item{\code{price}:}{Price by age and weight}
  }
}
\section{Methods}{
  \describe{

Summarise contents: 
    \item{summary}{\code{signature(object = "FLCatch")}: Prints a summary of the object contents}

Plotting contents
    \item{plot}{\code{signature(x = "FLCatch", y = "missing")}: Generates a number of
    standard plots for an \code{FLCatch}. Several plot styles can be specified, by
    using the \code{type} parameter. If type="summary", a four-section plot is created 
    showing total landings, the discards weight, the landing numbers and the landing weights.
    This is the default plot when no type is specified. If type="composite", a three-section
    plots is created showing the time series of \code{catch}, the differences between
    measured and calculated catches, if any, and catch-at-age. "catch" creates a plot of the
    logarithm of the ratios of succesive catches of each cohort. The log-ratios belonging 
    to the same cohort are joined with a line.
    "Wt" creates a plot with the mean weigth-at-age, a smooth line, calculated with the
    \code{\link{lowess}} function, is added for each age. "Wtres" creates a plot of the
    residuals (the log-ratio of the smooth line fitted weigth-at-age and observed values).
    }


Indexing and subsetting:
    \item{\link{window}:}{ To specify a temporal window, along the year dimension,
		over which to subset or extend the object.}

Other:    
    \item{catch}{\code{signature(object = "FLStock")}: \code{catch} provides a mechanism for
	estimating catch slots from landings and discards. Three options are available, to estimate
	catch.n from landings.n and discards.n (slot="n"), to estimate catch weights, catch.wt, as
	a weighted average of the discards and landings weights ate age (slot="wt"), and to calculate
	the catch time series from the previous two (slot="all").}
    \item{landings, discards}{\code{signature(object = "FLStock"):} These two methods estimate
	landings and discards time series respectively, from the corresponding weights and numbers.}
    \item{transform}{\code{signature(obj = "FLCatch")}: Alters the content of individual slots in
	a FLCatch object. Only the data is altered, not dimnames.}

  }
}
\author{FLR Team}
\seealso{
  \code{\link{FLFleet-class}}
}
\examples{
}
\keyword{classes}
\keyword{methods}
