% $Id: FLStock-class.Rd,v 1.4.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{FLStock-class}
\docType{class}
\alias{FLStock}
\alias{FLStock-class}
\alias{plot,FLStock,missing-method}
\alias{project,FLStock,FLSTF.control-method}
\alias{summary,FLStock-method}
\alias{update,FLStock-method}
\alias{catch,FLStock-method}
\alias{landings,FLStock-method}
\alias{discards,FLStock-method}
\alias{setunits,FLStock-method}
\alias{transform,FLStock-method}
\alias{ssb,FLStock-method}
\alias{harvest}
\alias{harvest<-}
\alias{harvest-methods}
\alias{"harvest<-"-methods}
\alias{harvest,FLStock-method}
\alias{harvest<-,FLStock-method}
\alias{apply,FLStock,list,function-method}
\alias{harvest<-,FLStock,character-method}
\title{Class FLStock for fish stock data and modelling output}
\description{
    The \code{FLStock} class is a data representation of a fish stock. Slots
    in this class can store most relevant data, such as catch and weight by quant.
    In can also be updated to include quantities estimated by stock assessment
    models, such as fishing mortality and numbers at quant.
}
\section{Objects from the Class}{
    Objects can be created by calls to the constructor function \code{FLStock()}
    or by \code{new("FLStock", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{A character vector with the name of the stock}
    \item{\code{desc}:}{A description of the stock}
    \item{\code{range}:}{Year and quant range, and specified plus group quant (if required). A vector of length 5.}
    \item{\code{catch}:}{An FLQuant holding the total catch (landings plus discards)
    in weight for all quants}
    \item{\code{catch.n}:}{Catch in numbers by quant}
    \item{\code{catch.wt}:}{Weight at quant in the catch, usually obtained from those for landings and discards (see \code{update})}
    \item{\code{discards}:}{An FLQuant holding the total discards in weight for all quants}
    \item{\code{discards.n}:}{Discards by quant in numbers}
    \item{\code{discards.wt}:}{Weight at quant of the discards}
    \item{\code{landings}:}{An FLQuant of the total landings for all quants}
    \item{\code{landings.n}:}{Landings in numbers by quant}
    \item{\code{landings.wt}:}{Weight at quant of landings}
    \item{\code{stock}:}{Stock weight for all quants. Usually obtained from an assessment method}
    \item{\code{stock.n}:}{Stock numbers by quant, output from an quant-structured assessment method}
    \item{\code{stock.wt}:}{Weights at quant in the stock}
    \item{\code{m}:}{Natural mortality by quant}
    \item{\code{mat}:}{Maturity by quant, usually as a proportion}
    \item{\code{harvest}:}{Harvest mode rate by quant}
    \item{\code{harvest.spwn}:}{Fraction of the harvest mode ocurring before spawning}
    \item{\code{m.spwn}:}{Fraction of the natural mortality ocurring before spawning}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "FLStock", y = "missing")}: Generates a number of
    standard plots for an \code{FLStock}. Five plots styles can be specified, by
    using the \code{type} parameter. If type="composite", a three-section plots is
    created showing the time series of \code{catch}, the differences between measured
    and calculated catches, if any, and catch-at-quant. This is the default plot when no
    type is specified. "catch" creates a plot of the logarithm of the ratios of
    succesive catches of each cohort. The log-ratios belonging to the same cohort are
    joined with a line.
    
    "Wt" creates a plot with the mean weigth-at-quant, a smooth line, calculated with the
    \code{\link{lowess}} function, is added for each quant. "Wtres" creates a plot of the
    residuals (the log-ratio of the smooth line fitted weigth-at-quant and observed values).
    "ts" creates a triple plot, with the time series of Recruitment, Spawning Stock Biomass
    (SSB) and predicted Fishing mortality (Fhat).
    }
    \item{project}{\code{signature(obj = "FLStock", control = "FLSTF.control")}: ... }
    \item{summary}{\code{signature(object = "FLStock")}: Prints a detailed summary of the
    slots in the object, indicating whether they contain any data and the number of years,
    quants, areas, units and seasons specified.
    }
    \item{catch}{\code{signature(object = "FLStock")}: \code{catch} provides a mechanism for
	estimating catch slots from landings and discards. Three options are available, to estimate
	catch.n from landings.n and discards.n (slot="n"), to estimate catch weights, catch.wt, as
	a weighted averquant of the discards and landings weights ate quant (slot="wt"), and to calculate
	the catch time series from the previous two (slot="all").}
    \item{landings, discards}{\code{signature(object = "FLStock"):} These two methods estimate
	landings and discards time series respectively, from the corresponding weights and numbers.}
    \item{ssb}{ \code{signature(object = "FLStock"):} This creates an aggregated object of class 
	\code{\link{FLQuant-class}} containg the SSB, calculated depending on the relevant units 
	in the harvest slot.}
    }
	\item{setunits}{\code{signature(obj = "FLStock")}: Sets the units of all FLQuant slots in an
	FLStock object.}
	\item{transform}{\code{signature(obj = "FLStock")}: Alters the content of individual slots in
	a FLStock object. Only the data is altered, not dimnames.}
    \item{as.FLStock}{\code{signature(obj = "FLStock")}: Outputs a new FLStock using the input specified as base with the options changed as specified.}
	\item{apply}{\code{signature(obj = "FLStock", "list", "function")}: Usual apply function.}
	\item{harvest<-}{\code{signature(obj = "FLStock", "character")}: Changes the units of the harvest FLQuant slot.}
  }
}
\author{FLR Team}
\seealso{
    \code{\link{as.FLStock}}, \code{\link{FLQuant-class}}
}
\examples{
# Create a new empty stock, giving a name and desc.
my.ple <- FLStock(name = "plaice", desc = "Just an example...")
summary(my.ple)

# Create an FLStock with the same dimensions as the FLQuant object catch.
catch.n <- FLQuant(array(round(rnorm(120, 5000,500)), dim=c(12,10,1,1,1),
          dimnames=list(quant=as.character(1:12), year=as.character(c(1991:2000)),
          unit="all", season="all", area="all")))
my.stock <- FLStock(name = "My Stock", desc = "Just an example...", iniFLQuant=catch.n)
}
\keyword{classes}
