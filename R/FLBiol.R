# FLBiol - «Short one line description»
# Author: FLR Team
# Maintainer: Laurie Kell, CEFAS
# Additions:
# Last Change: 26 mar 2006 18:15
# $Id: FLBiol.R,v 1.14.2.8 2006/03/28 07:19:58 iagoazti Exp $
# Reference:
# Notes:
# TODO Lun 07 Mar 2005 12:10:37 GMT iagoazti:

## class::FLBiol
validFLBiol <- function(object){

   ## All FLQuant objects must have same dimensions
   Dim <- dim(object@n)

   s.  <-list("n","wt","fec","spwn","m")

   for (i. in s.)	{
	   t. <- slot(object,i.)
	   if (is.FLQuant(t.) & !all(dim(t.) == Dim))
	      return(paste("FLQuant dimensions wrong for ", i.))
	   }

   # Verify that bounds are correct and correspond to first slot
	.t  <-getSlots(class(object))
	.t  <-.t[.t=="FLQuant"]

   if (length(.t)> 0) {

      Par <- dims(.s<-slot(object,names(.t[1])))

	   min <- object@range["min"]
      if (!is.na(min) && (min < Par$min || min > Par$max))
   		return("min quant is outside range in FLQuant slots")
	   max <- object@range["max"]
	   if (!is.na(max) && (max < Par$min || max > Par$max))
	   	return("max quant is outside range in FLQuant slots")
	   if (!is.na(min) && !is.na(max) && max < min)
		   return("max quant is lower than min")
	   plusgroup <- object@range["plusgroup"]
	   if (!is.na(plusgroup) && (plusgroup < Par$min || plusgroup > Par$maxage))
		   return("plusgroup is outside [min, max] quant range in FLQuant slots")
	   minyear <- object@range["minyear"]
	   if (!is.na(minyear) && (minyear < Par$minyear || minyear > Par$maxyear))
		   return("minyear is outside years range in FLQuant slots")
	   maxyear <- object@range["maxyear"]
	   if (!is.na(maxyear) && (maxyear < Par$minyear || maxyear > Par$maxyear))
		   return("maxyear is outside years range in FLQuant slots")
	   if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
		   return("maxyear is lower than minyear")
	   }

   # Everything is fine
   return(TRUE)
   }

setClass("FLBiol",
	representation(
        name     ="character",
        desc     ="character",
        range    ="numeric",
        n        ="FLQuant",
		m        ="FLQuant",
		wt       ="FLQuant",
		fec      ="FLQuant",
		spwn     ="FLQuant"
      ),
	prototype=prototype(
		name     =character(0),
		desc     =character(0),
		range    =unlist(list(minage=NA, maxage=NA, plusgroup=NA, minyear=1, maxyear=1)),
        n        = FLQuant(),
		m        = FLQuant(),
		wt       = FLQuant(),
		fec      = FLQuant(),
		spwn     = FLQuant()),
	validity=validFLBiol
)

setValidity("FLBiol", validFLBiol)
remove(validFLBiol)	# We do not need this function any more

FLBiol <- function(name=character(0), desc=character(0), plusgroup=NA, quant='quant', ...){

	args <- list(...)

	if(length(args)==0)
		args <- list(n=FLQuant())

	# Set FLQuant dimensions
	dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])

	# template FLQuants
	if (missing(quant))
		quant <- names(dimnames)[1]
	iniFLQ <- FLQuant(dimnames=dimnames, quant=quant)
	agrFLQ <- FLQuant(dimnames=c(quant='all', dimnames(iniFLQ)[-1]), quant=quant)

	dims <- dims(iniFLQ)

	res <- new("FLBiol",
		name		= name,
		desc		= desc,
        n           = iniFLQ,
		m	        = iniFLQ,
		wt	        = iniFLQ,
		fec		    = iniFLQ,
		spwn	    = iniFLQ,
		range       = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear)))
	
	# Load given slots
	for(i in names(args))
		if (i != 'iniFLQuant')
			slot(res, i) <- args[[i]]

	return(res)
}	# }}}

## summary
setMethod("summary", signature(object="FLBiol"),
	function(object, ...){
		cat("An object of class \"FLBiol\"\n\n")
		cat("Name:", object@name, "\n")
		cat("Description:", object@desc, "\n")
		cat("Range:\tmin\tmax\tp+group\tminyear\tmaxyear\n")
		cat("", object@range, "\n", sep="\t")
		cat("Quant:", quant(object@catch), "\n\n")
		
		for (s in names(getSlots("FLBiol")[getSlots("FLBiol")=="FLQuant"])) {
				 if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
     				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				    cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
		        }
	}
)

# Test if an object is of FLBiol class
is.FLBiol <- function(x)
	return(inherits(x, "FLBiol"))


## window
if (!isGeneric("window")) {
	setGeneric("window", function(x, ...){
		value  <-  standardGeneric("window")
		value
	})
}
setMethod("window", signature(x="FLBiol"),
	  function(x, start, end, extend=TRUE, frequency=1) {

		names. <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		
		for (s. in names.) {
 	            slot(x, s.) <- window(slot(x, s.), start=start, end=end,
		      extend=extend, frequency=frequency)

		}
  	        x@range["minyear"] <- start
		x@range["maxyear"] <- end

		return(x)
	}
)


setMethod("transform", signature(`_data`="FLBiol"),
	function(`_data`, ...) {

		args <- list(...)
		for (i in 1:length(args)) {
			slot(`_data`, names(args)[i]) <- args[[i]]
		}

		return(`_data`)
	}
)

## apply(FLBiol)
setMethod("apply", signature(X="FLBiol", MARGIN="list", FUN="function"),
	function(X, MARGIN, FUN, ...) {

		for(i in 1:length(MARGIN)) {
			slot(X, MARGIN[[i]]) <- apply(slot(X, MARGIN[[i]]), 1:5, FUN, ...)
		}
		return(X)
	}
)

## as.FLBiol
if (!isGeneric("as.FLBiol")) {
	setGeneric("as.FLBiol", function(object, ...){
		value <- standardGeneric("as.FLBiol")
		value
	})
}

setMethod("as.FLBiol", signature(object="FLBiol"),

  function(object, unit  =1:dim(object@n)[3],
                   season=1:dim(object@n)[4],
                   area  =1:dim(object@n)[5]) {

    slotnames <- names(getSlots("FLBiol")[getSlots("FLBiol")=="FLQuant"])
    for(slotname in slotnames){
      s.d <- dim(slot(object, slotname))
      slot(object, slotname) <- slot(object, slotname)[,,pmin(unit,s.d[3]),
                                                         pmin(season,s.d[4]),
                                                         pmin(area,s.d[5])]
    }
    return(object)
  }
)

# PLOT

setMethod("plot", signature(x="FLBiol", y="missing"),
	function(x, y, ...){
		ssb <- quantSums(x@n*x@fec)
		rec <- x@n[1,,,,]
		flqs <- FLQuants(list(ssb=ssb, rec=rec))
		condnames <- names(dimnames(x@n)[c(3:5)][dim(x@n)[c(3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|age*", cond)# else cond <- paste("|age") 
		formula <- formula(paste("data~year", cond))
		xyplot(x=formula, data=flqs, auto.key=T, type="b", ...)

	}
)

## Accesors
invisible(createFLAccesors(new("FLBiol"), exclude='range'))
