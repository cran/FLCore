% $Id: FLStock-class.Rd,v 1.4.2.9 2006/04/26 16:08:52 ejardim Exp $
\name{FLStock-class}
\docType{class}
\alias{FLStock}
\alias{FLStock-class}
\alias{plot,FLStock,missing-method}
%\alias{project,FLStock,FLSTF.control-method}
\alias{trim,FLStock-method}
\alias{summary,FLStock-method}
\alias{update,FLStock-method}
\alias{catch,FLStock-method}
\alias{landings,FLStock-method}
\alias{discards,FLStock-method}
\alias{units,FLStock,list-method}
\alias{transform,FLStock-method}
\alias{ssb,FLStock-method}
\alias{name,FLStock-method}
\alias{desc,FLStock-method}
\alias{range,FLStock-method}
\alias{catch,FLStock-method}
\alias{catch.n,FLStock-method}
\alias{catch.wt,FLStock-method}
\alias{discards,FLStock-method}
\alias{discards.n,FLStock-method}
\alias{discards.wt,FLStock-method}
\alias{landings,FLStock-method}
\alias{landings.n,FLStock-method}
\alias{landings.wt,FLStock-method}
\alias{stock,FLStock-method}
\alias{stock.n,FLStock-method}
\alias{stock.wt,FLStock-method}
\alias{m,FLStock-method}
\alias{mat,FLStock-method}
\alias{harvest,FLStock-method}
\alias{harvest.spwn,FLStock-method}
\alias{ssbpurec,FLStock-method}
\alias{m.spwn,FLStock-method}
\alias{name<-,FLStock,character-method}
\alias{desc<-,FLStock,character-method}
\alias{range<-,FLStock,numeric-method}
\alias{catch<-,FLStock,FLQuant-method}
\alias{catch<-,FLStock,FLQuants-method}
\alias{catch.n<-,FLStock,FLQuant-method}
\alias{catch.wt<-,FLStock,FLQuant-method}
\alias{discards<-,FLStock,FLQuant-method}
\alias{discards.n<-,FLStock,FLQuant-method}
\alias{discards.wt<-,FLStock,FLQuant-method}
\alias{landings<-,FLStock,FLQuant-method}
\alias{landings.n<-,FLStock,FLQuant-method}
\alias{landings.wt<-,FLStock,FLQuant-method}
\alias{stock<-,FLStock,FLQuant-method}
\alias{stock.n<-,FLStock,FLQuant-method}
\alias{stock.wt<-,FLStock,FLQuant-method}
\alias{m<-,FLStock,FLQuant-method}
\alias{mat<-,FLStock,FLQuant-method}
\alias{harvest<-,FLStock,FLQuant-method}
\alias{harvest<-,FLStock,character-method}
\alias{harvest.spwn<-,FLStock,FLQuant-method}
\alias{m.spwn<-,FLStock,FLQuant-method}
\alias{apply,FLStock,vector,function-method}
\alias{harvest<-,FLStock,character-method}
\title{Class FLStock for fish stock data and modelling output}
\description{
    The \code{FLStock} class is a data representation of a fish stock, that is, the
	scientific perception of a natural population. Slots in this class store most of 
	the relevant data, such as catch (as landings, discards and total catch), or mean
	weight in the catch by quant (age, length or other).
    In can also be updated to include quantities estimated by stock assessment
    models, such as fishing mortality and abundance.

	Catch and abundance slots store three related quantities: numbers by quant, mean
	weights by quant, and total biomass agreggated along the quant dimension. Subsequently,
	all slots of class \code{\link{FLQuant-class}} are of the same dimensions, except for
	the agreggated slots (\code{catch}, \code{landings}, \code{discards} and \code{stock})
	that have lenght=1 on the first dimension.

	The \code{harvest} slot holds the estimated values of fishing mortality. Use of the units
	attribute in very important in this slot, as its content might refer to fishing mortality
	(F) or harvest rate. Many methods will only work if the expected units are specified.
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
    \item{\code{catch.wt}:}{Weight at quant in the catch, usually obtained from those for landings and discards (see \code{computeCatch})}
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
    \item{plot}{\code{signature(x = "FLStock", y = "missing")}: Generates a standard
	exploratory plot for an \code{FLStock}. The time series of catch, landings and
	discards is presented, conditioned on units, areas or season when relevant.
    }
%    \item{project}{\code{signature(obj = "FLStock", control = "FLSTF.control")}: ... }
    \item{summary}{\code{signature(object = "FLStock")}: Prints a detailed summary of the
    slots in the object, indicating whether they contain any data and the number of years,
    quants, areas, units and seasons specified.
    }
    \item{computeLandings, computeDiscards}{\code{signature(object = "FLStock")}:
	Returns an FLQuant with the calculated aggregated slots for landings and discards
	respectively from numbers and mean weights.}
	\item{computeCatch}{\code{signature(object = "FLStock")}: Computes the total catch in
	weigth from catch numbers (\code{catch.n}) and mean weight in the catch (\code{catch.wt}),
	when \code{slot="catch"}. If \code{slot="n"} or \code{slot="wt"}, catch numbers or mean
	weight in the catch will be calculated from landings and discards. Finally, if
	\code{slot="catch"}, all three calculations will be performed and an object of class
	\code{\link{FLQuants-class}} will be returned.}
    \item{ssb}{ \code{signature(object = "FLStock"):} This creates an aggregated object of class 
	\code{\link{FLQuant-class}} containg the SSB, calculated depending on the relevant units 
	in the harvest slot.}
	\item{ssbpurec}{\code{signature(obj = "FLStock")}: Computes, parameterically or 
	non-parametrically, the SSB-per-unit-recruit at the FLQuant resolutioni and 
	returns an object of class FLQuant.} 
	\item{setunits}{\code{signature(obj = "FLStock")}: Sets the units of all FLQuant slots in an
	FLStock object.}
	\item{transform}{\code{signature(`_data` = "FLStock")}: Alters the content of individual slots in
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
my.stock <- FLStock(name = "My Stock", desc = "Just an example...", catch.n=catch.n)
}
\keyword{classes}
