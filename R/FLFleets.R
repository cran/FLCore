# FLFleets.R - FLFleets class and methods

# Author: Phillipe Grosjean, Laurence T. Kell.
# Additions:
# Last Change: 14 Dec 2005 23:18
# $Id: FLFleets.R,v 1.8.2.3 2005/12/19 11:22:22 iagoazti Exp $

# Reference:
# Notes:

## class :: FLFleets
validFLFleets <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)

	# Make sure the list contains only FLFleet items
	for (i in 1:length(object))
		if (!inherits(object[[i]], "FLFleet"))
			return("Items must be FLFleet objects!")

	res <- validObject(object[[i]])
	if (!res)
		return("Fleet", i, "is not valid:", res)

	# Everything is fine
	return(TRUE)
}

setClass("FLFleets",
	representation(
		"list",# Indeed, a list of FLFleet objects
		desc ="character"),
	prototype=prototype(
		list(),
		desc =character(0)),
	validity=validFLFleets
)

setValidity("FLFleets", validFLFleets)
remove(validFLFleets)

## FLFleets()
# The creator (better than using new(...) directly!
FLFleets <- function(..., desc){
	if (missing(desc))
		desc <- character(0)
	# Look at the ... argument: should be various FLFleet objects
	Fleets <- list(...)
	# ... could be already a list of FLFleets objects
	if (length(Fleets) == 1 && inherits(Fleets[[1]], "list"))
		Fleets <- Fleets[[1]]
	if (length(Fleets) > 0)
		for (i in 1:length(Fleets))
			if (!inherits(Fleets[[i]], "FLFleet"))
				stop("Variables provided in ... must all be FLFleet objects!")

	return(new("FLFleets", Fleets, desc=desc))
}

## is.FLFLeets
# Test if an object is of FLStock class
is.FLFleets <- function(x)
	return(inherits(x, "FLFleets"))

## show (the default method)
# Rem: not needed...
#setMethod("show", signature(object="FLFleets"),
#	function(object){
#		cat("An object of class \"FLFleets\":\n\n")
#		# Add more info here?
#		print(unclass(object))
#	}
#)

## summary
setMethod("summary", signature(object="FLFleets"),
	function(object, ...){
		cat("An object of class \"FLFleets\" with:\n\n")
		cat("Description:", object@desc, "\n")
		cat("and containing", length(object), "Fleets:\n")
		if (length(object) > 0)
			for (i in 1:length(object))
				cat("Fleet", i, ":", object[[i]]@name, "- method =", object[[i]]@method, "\n")
		# Should we add some summary stats for the different slots here?
	}
)

## as.FLStock::FLFleets
setMethod("as.FLStock", signature(object="FLFleets"),
    function(object, name="missing", gear="missing", ...) {
        # If only one stock in @catch and stock=missing, take it
        if (missing(name)) print("name argument is required in as.FLStock(FLFleets)")
        # If stock is character, look for it in @catch
        else if (is.character(name)) {
            #check which fleets have catch slots with name=name, fllstock is vector with fleets,
            #indstock is vector with catches
            fllstock <- vector()
            for (i in 1:length(object)) {
                indstock <- vector()
                if (is.character(name) && missing(gear)){
                    for(j in 1:length(object[[i]]@catches)) if (object[[i]]@catches[[j]]@name == name) indstock[j] <- j
                }
                else if (missing(name) && is.character(gear)){
                    for(j in 1:length(object[[i]]@catches)) if (object[[i]]@catches[[j]]@gear == gear) indstock[j] <- j
                }
                else if (is.character(name) && is.character(gear)){
                    for(j in 1:length(object[[i]]@catches)) if ((object[[i]]@catches[[j]]@gear == gear) && (object[[i]]@catches[[j]]@gear == gear)) indstock[j] <- j
                }
                indstock <- indstock[!is.na(indstock)]
                if (length(indstock)!=0) fllstock[i] <- i
            }
            fllstock <- fllstock[!is.na(fllstock)]
            if (length(fllstock)==0) stop(paste("no fleets with satisfying arguments can be found in fleets by as.FLStock()"))
        } else
            stop("name must be a character string ")
        # Output stock object
        #   Uses landings.n  in first fleet for sizing stock, but it might be
        #   missing.
        # now, fllstock contains the fleets which need to be put in new stock object.
        # if length of fllstock==1 then simple use of as.FLStock(FLFleet). If length >1 then calculations are needed
        if (length(fllstock)==1){
            if (missing(gear)) fls <- as.FLStock(object[[fllstock[1]]],name=name)
            else  fls <- as.FLStock(object[[fllstock[1]]],name=name, gear=gear)
        } else { #length of fllstock larger than one meaning multiple fleets have to be combined
            if (missing(gear)) fls <- as.FLStock(object[[fllstock[1]]],name=name)
            else  fls <- as.FLStock(object[[fllstock[1]]],name=name, gear=gear)
            slot(fls, "catch.wt")    <- slot(fls, "catch.wt")    * slot(fls, "catch.n")
            slot(fls, "landings.wt") <- slot(fls, "landings.wt") * slot(fls, "landings.n")
            slot(fls, "discards.wt") <- slot(fls, "discards.wt") * slot(fls, "discards.n")

            for (fll in 2:length(fllstock)){
                if (missing(gear)) flsn <- as.FLStock(object[[fllstock[fll]]],name=name)
                else  flsn <- as.FLStock(object[[fllstock[fll]]],name=name, gear=gear)
                for (sl in c("catch", "catch.n", "landings", "landings.n", "discards", "discards.n")) {
                      slot(fls, sl) <- slot(fls,sl) + slot(flsn, sl)
                }
                slot(fls, "catch.wt") <- slot(fls,"catch.wt") + slot(flsn, "catch.wt") * slot(flsn, "catch.n")
                slot(fls, "landings.wt") <- slot(fls,"landings.wt") + slot(flsn, "landings.wt") *  slot(flsn, "landings.n")
                slot(fls, "discards.wt") <- slot(fls,"discards.wt") + slot(flsn, "discards.wt") *  slot(flsn, "discards.n")
            }
            # if done adding, complete weighted averaging by dividing weights by numbers
            slot(fls, "catch.wt")    <- slot(fls,"catch.wt")    / slot(fls,"catch.n")
            slot(fls, "landings.wt") <- slot(fls,"landings.wt") / slot(fls, "landings.n")
            slot(fls, "discards.wt") <- slot(fls,"discards.wt") / slot(fls, "discards.n")
        }
        # give name to stock and return it
        fls@name <- paste("combinaton of catch slots derived from FLfleets, with name" , name, "and gear", gear )
        return(fls)
   }
) # }}}
