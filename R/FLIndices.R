# FLIndices.R - FLIndices class and methods

# Author: FLR Team
# Additions:
# Last Change: 15 Dec 2005 11:38
# $Id: FLIndices.R,v 1.3.2.1 2005/12/19 11:22:22 iagoazti Exp $

# Reference:
# Notes:

## class :: FLIndices
validFLIndices <- function(object){
    # If the list is empty, then it is OK
	if (length(object) == 0)
	    return(TRUE)
        # Make sure the list contains only FLIndex items
	    for (i in 1:length(object))
		    if (!inherits(object[[i]], "FLIndex"))
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
		    if (!inherits(Indexs[[i]], "FLIndex"))
			    stop("Variables provided in ... must all be FLIndex objects!")
    return(new("FLIndices", Indexs, desc=desc))
}

## is.FLCPUes
# Test if an object is of FLIndices class
is.FLIndices <- function(x)
    return(inherits(x, "FLIndices"))

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

