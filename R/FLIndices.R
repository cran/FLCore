# FLIndices.R - FLIndices class and methods

# Author: FLR Team
# Maintainer: Richard Hillary, Imperial College London
# Additions:
# Last Change: 23 jun 2006 17:24
# $Id: FLIndices.R,v 1.3.2.9 2006/07/13 10:13:35 iagoazti Exp $

# Reference:
# Notes:

## class :: FLIndices
validFLIndices <- function(object){
    # If the list is empty, then it is OK
	if (length(object) == 0)
	    return(TRUE)
        # Make sure the list contains only FLIndex items
	    for (i in 1:length(object))
		    if (!is.FLIndex(object[[i]]))
			    return("Items must be FLIndex objects!")
		    res <- validObject(object[[i]])
		    if (!res)
			    return("Index", i, "is not valid:", res)
	    # Everything is fine
    return(TRUE)
}

setClass("FLIndices", 
    representation(
    "list",# Indeed, a list of FLIndex objects
        desc ="character"),
    prototype=prototype(
		list(),
		desc =character(0)),
	validity=validFLIndices
)

setValidity("FLIndices", validFLIndices)
remove(validFLIndices)	# We do not need this function any more


## Methods :: FLIndices

# FLIndices()
FLIndices <- function(..., desc) {
    if (missing(desc))
        desc <- character(0)
    # Look at the ... argument: should be various FLIndex objects
    Indexs <- list(...)
    # ... could be already a list of FLIndices objects
    if (length(Indexs) == 1 && inherits(Indexs[[1]], "list"))
	    Indexs <- Indexs[[1]]
    if (length(Indexs) > 0)
	    for (i in 1:length(Indexs))
		    if (!is(Indexs[[i]], "FLIndex"))
			    stop("Variables provided in ... must all be FLIndex objects!")
    return(new("FLIndices", Indexs, desc=desc))
}

## is.FLIndices
# Test if an object is of FLIndices class
is.FLIndices <- function(x)
    return(is(x, "FLIndices"))

## summary
setMethod("summary", signature(object="FLIndices"),
    function(object, ...){
        cat("An object of class \"FLIndices\" with:\n\n")
	    cat("Description:", object@desc, "\n")
	    cat("and containing", length(object), "Indexs:\n")
	    if (length(object) > 0)
		    for (i in 1:length(object))
			    cat("Index", i, ":", object[[i]]@name, "- type =", object[[i]]@type, "\n")
	   # TODO iagoazti 20/11/2004 add some summary stats for the different slots
   }
)

## window :: FLIndices

setMethod("window", signature(x="FLIndices"),
	function(x, start, end, extend=TRUE, frequency=1) {
		for (i in 1:length(x)) {
			names. <- names(getSlots(class(x[[i]]))[getSlots(class(x[[i]]))=="FLQuant"])
			for (s. in names.) {
				slot(x[[i]], s.) <- window(slot(x[[i]], s.), start=start, end=end,
					extend=extend, frequency=frequency)
				x[[i]]@range["minyear"] <- start
				x[[i]]@range["maxyear"] <- end
				# cat("Index", i, ":", names.)
			}
		}
		return(x)
	}
)

## plot (make specific plots to display units graphically)  {{{
setMethod("plot", signature(x="FLIndices", y="missing"),
	function(x, ylab="standardised index", ...){
    dps <- NULL
    res <- list()
    for(i in 1:length(x)){
      res[[i]] <- sweep(x[[i]]@index,1,apply(x[[i]]@index,  1,mean,na.rm=T),"/")
      d <- x[[i]]@name[[1]]
      dpsd <- cbind(as.data.frame(res[[i]]),d)
      dps <- rbind(dps, as.data.frame(dpsd))
    }
    dps$year <- as.numeric(as.character(dps$year))
    dps$age <- as.factor(dps$age)
    print(xyplot(data~year|age,outer=T, group=dps$d, type="b", data=dps,
      key = list(columns=length(levels(dps$d)),text=list(levels(dps$d)), col="black",
      points=Rows(trellis.par.get("superpose.symbol"),1:length(levels(dps$d)))),
      scales=list(relation="sliced",draw=TRUE),lty=(5:1),lwd=1.5,
      ylab=ylab, ...))
	}
)	# }}}

## trim     {{{
setMethod("trim", signature("FLIndices"), function(object, ...){

    for (i in seq(along.with=object))
        object[[i]] <- trim(object[[i]], ...)
    return(object)
    }
)   # }}}

## "["             {{{
setMethod("[", signature(x="FLIndices"),
	function(x, i="missing", j="missing",..., drop="missing") {

		if (missing(i))
			i  <-  seq(1, length(x))
        res <- new('FLIndices', x@.Data[i])
        names(res) <- names(x)[i]
   		return(res)
	}
)   # }}}
