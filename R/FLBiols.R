#########################################################################
# FLBiols.R - FLBiols class and methods

# Author: Martin Pastoors
# Additions:
# Last Change: 28 Mai 2005 09:43

# Reference:
# Notes:
### class ##############################################################

validFLBiols <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)
	# Make sure the list contains only FLBiol item
	for (i in 1:length(object))
		if (!inherits(object[[i]], "FLBiol"))
			return("Items must be FLBiol objects!")
	# Everything is fine
	return(TRUE)
}

setClass("FLBiols",
	representation(
		"list",# Indeed, a list of FLBiol objects
		desc ="character",
		names="character"),
	prototype=prototype(
		list(),
		desc =character(0),
		names=character(0)),
	validity=validFLBiols
)

setValidity("FLBiols", validFLBiols)
remove(validFLBiols)	# We do not need this function any more

### End class ###########################################################

### Constructor #########################################################

FLBiols <- function(..., desc=NULL, names=NULL) {

    new <- new("FLBiols")

    objects <- list(...)

    if(!is.null(desc))
        new@desc <- desc

    if (length(objects) > 0) {
        for (n in 1:length(objects)) {
            object <- objects[[n]]
            if (!inherits(object, "FLBiol"))
			    stop("input must be one or more FLBiol objects")
            new[[n]] <- object
			if(!is.null(object@spp))
				names(new)[n] <- object@spp
        }
    }
	if (!missing(names))
		names(new) <- names

    return(new)
}

### Methods #############################################################

# show (the default method)
setMethod("show", signature(object="FLBiols"),
	function(object){
		cat("An object of class \"FLBiols\":\n\n")
		print(unclass(object))
	}
)

## summary :: FLBiols
setMethod("summary", signature(object="FLBiols"),
	function(object, ...){
		cat("An object of class \"FLBiols\" with:\n\n")
		cat("Description:", object@desc, "\n\n")
		cat("Containing", length(object), "biols:\n")
		if (length(object) > 0)
			for (i in 1:length(object))
				cat("Biol", i, ":", object[[i]]@name, "\n")
		# Should we add some summary stats for the different slots here?
	}
)

# Test if an object is of FLStock class
is.FLBiols <- function(x)
	return(inherits(x, "FLBiols"))

### End methods ###########################################################
