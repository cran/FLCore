\name{FLCore-package}
\alias{FLCore-package}
\alias{FLCore}
\docType{package}
\title{
Core package of FLR, fisheries modelling in R.
}
\description{
FLCore contains the core classes and methods for FLR, a
framework for fisheries modelling and management strategy
simulation in R. Developed by a team of fisheries scientists in
various countries. More information can be found at
http://flr-project.org/, including a development mailing list.
}
\details{
\tabular{ll}{
Package: \tab FLCore\cr
Version: \tab 2.0-2\cr
Date: \tab 2005\cr
Depends: \tab methods, R(>= 2.3.0), graphics, stats, lattice(>= 0.13-8)\cr
License: \tab GPL 2 or above\cr
Collate: \tab FLQuant.R FLQuants.R FLAccesors.R FLCohort.R FLStock.R
FLStocks.R io.FLStock.R FLBiol.R FLBiols.R FLCatch.R FLFleet.R
FLFleets.R FLIndex.R FLIndices.R io.FLIndices.R FLSR.R zzz.R\cr
Packaged: \tab Sat May 13 10:25:52 2006; imosqueira\cr
Built: \tab R 2.3.1; ; 2006-10-11 12:26:16; unix\cr
}

Index:
\preformatted{
FLBiol-class            Class FLBiol
FLBiols-class           Class FLBiols
FLCatch-class           Class FLCatch
FLCohort-class          Class "FLCohort" for information by cohort
FLCore-accesors         Accesor and replacement methods for slots of
                        complex objects
FLFleet-class           FLFleet class and methods
FLFleets-class          Class FLFleets
FLGenerics-methods      FLCore S4 Generic Functions
FLIndex-class           Class FLIndex
FLIndices-class         Class FLIndices
FLQuant                 FLQuant
FLQuant-class           FLQuant class and methods
FLQuants-class          Class "FLQuants"
FLSR                    FLSR
FLSR-class              Class FLSR
FLStock-class           Class FLStock for fish stock data and modelling
                        output
FLStocks-class          Class FLStocks
as.FLBiol               Method for creating an FLBiol object from other
                        classes
as.FLFleet              as.FLFleet method
as.FLIndex-methods      Method for creating an FLIndex object from
                        other classes
as.FLQuant              as.FLQuant
as.FLSR                 Method for creating an FLSR object from other
                        classes.
as.FLStock              FLStock
bubbles-methods         Lattice style bubble plots
ccplot-methods          Catch curves plot method
computeCatch            Methods for estimating aggregated catch time
                        series from dissagregated data
convert6d               Converts FLR objects from version 1.* to 2.*
createFLAccesors        Create accesor and replecement methods for
                        slots of complex classes
dims                    Provide information onn the dimensions and
                        range of an object
flc2flq-methods         Coerce "FLCohort" to "FLQuant"
is.FLBiol               FLBiol
is.FLBiols              Checks for objects of class FLBiols
is.FLFleet              FLFleet
is.FLFleets             FLFleets
is.FLIndex              FLIndex
is.FLIndices            FLIndices
is.FLSR                 FLSR
is.FLStock              FLStock
is.FLStocks             FLStocks
iter-methods            Methods for getting information on, accessing
                        and modifying iterations of an FLQuant
ple4                    FLCore datasets
propagate               propagate for FLQuants based classes
quant                   Quant, first dimension of FLQuant
quantSums               Common summary operations for FLQuants
read.FLIndices          Import FLIndices data from a file
read.FLStock            Import stock data from a file
revenue                 Calculate the revenue of a fleet
setPlusGroup            setPlusgroup for FLStock
sop                     Calculates the sum of products correction
sr                      Stock-recruitment model function
srlkhd                  Likelihood of the S-R parameters
ssb                     Method for calculating Spawning Stock Biomass
ssbpurec                Method for calculating Spawning Stock Biomass
                        per unit recruit
steepvirginbiom         Change to steepness/virgin biomass
                        parameterisation
tofrm                   Method for generating fromulas from FLQuant
                        objects.
trim                    Method for trimming FLQuant objects.
units                   Extract and modify the units slot of an object
window                  window for FLQuants based classes
write.FLStock           Write FLStock data to a file
xyplot,formula,FLQuant-method
                        FLCore lattice methods
}
}
\author{
FLR Team and various contributors. Initial design by Laurence
T. Kell & Philippe Grosjean.

Maintainer: FLR Team <flr-devel@lists.sourceforge.net>
}
\references{
}
\keyword{package}
\seealso{
}
\examples{
}
