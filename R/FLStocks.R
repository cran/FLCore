# FLStocks.R - FLStocks class and methods

# Author: FLR Team
# Maintainer: Rob Scott, CEFAS
# Additions:
# Last Change: 26 mar 2006 18:18
# $Id: FLStocks.R,v 1.5.2.2 2006/03/28 07:19:58 iagoazti Exp $

# Reference:
# Notes:

### class ##############################################################
validFLStocks <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)
	# Make sure the list contains only FLStock items
	for (i in 1:length(object))
		if (!inherits(object[[i]], "FLStock"))
			return("Items must be FLStock objects!")
	# Everything is fine
	return(TRUE)
}

setClass("FLStocks",
	representation(
		"list",# Indeed, a list of FLStock objects
		desc ="character"),
	prototype=prototype(
		list(),
		desc =character(0)),
	validity=validFLStocks
)

setValidity("FLStocks", validFLStocks)
remove(validFLStocks)	# We do not need this function any more
### End class ###########################################################

### FLStocks()

FLStocks <- function(..., desc=NULL) {

    new <- new("FLStocks")
 
    objects <- list(...)

    if(!is.null(desc))
        new@desc <- desc

    if (length(objects) > 0) {
        for (n in 1:length(objects)) {
            object <- objects[[n]]
            if (!inherits(object, "FLStock"))
			    stop("input must be one or more FLStock objects")
            new[[n]] <- object
        }
    }
    return(new)
}

### Methods #############################################################

## show (the default method)
# Rem: not needed...
#setMethod("show", signature(object="FLStocks"),
#	function(object){
#		cat("An object of class \"FLStocks\":\n\n")
#		# Add more info here?
#		print(unclass(object))
#	}
#)

## summary
setMethod("summary", signature(object="FLStocks"),
	function(object, ...){
		cat("An object of class \"FLStocks\" with:\n\n")
		cat("Description:", object@desc, "\n\n")
		cat("Containing", length(object), "stocks:\n")
		if (length(object) > 0)
			for (i in 1:length(object))
				cat("Stock", i, ":", object[[i]]@name, "\n")
		# Should we add some summary stats for the different slots here?
	}
)

### End methods ###########################################################

# Test if an object is of FLStock class
is.FLStocks <- function(x)
	return(inherits(x, "FLStocks"))

