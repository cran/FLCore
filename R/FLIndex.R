# FLIndex.R - FLIndex class and methods

# Author: FLR Team
# Additions:
# Last Change: 20 Dec 2005 13:52
# $Id: FLIndex.R,v 1.20.2.9 2005/12/20 15:40:25 iagoazti Exp $

# Reference:
# Notes:

## class :: FLIndex       {{{
validFLIndex <- function(object) {

	dim <- dim(object@index)
	dims <- dims(object@index)

    s <- list("index", "index.var")
    for (i in s) {
    	if(!is.FLQuant(slot(object, i)) || !all(dim(slot(object, i)) == dim)) 
                stop(paste("FLQuant dimensions wrong for ", i, "or object is not an FLQuant"))
        # min / max
        if(all(is.numeric(dimnames(object@index)[[quant(object@index)]]))) {

            min <- object@range["min"]
            if(!is.na(min) && (min < dims$min || min > dims$max))
                   stop(paste("min is outside quant range in FLQuant slot", i))
            max <- object@range["max"]
            if(!is.na(max) && (max < dims$min || max > dims$max))
                   stop(paste("max is outside quant range in FLQuant slot", i))
            if (!is.na(min) && !is.na(max) && max < min)
               stop(paste("max quant is lower than min quant in FLQuant slot", i))
        }
    }
    
    # plusgroup
    plusgroup <- object@range["plusgroup"]
    if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
        stop("plusgroup is outside [min, max] range in FLQuant slots")

    # minyear / maxyear
    minyear <- object@range["minyear"]
    if (!is.na(minyear) && (minyear < dims$minyear || minyear > dims$maxyear))
        stop(paste("minyear is outside years range in FLQuant slot", i))
    maxyear <- object@range["maxyear"]
    if (!is.na(maxyear) && (maxyear < dims$minyear || maxyear > dims$maxyear))
        stop(paste("maxyear is outside years range in FLQuant slot", i))
    if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
        stop(paste("maxyear is lower than minyear in FLQuant slot", i))
    
    # Everything is fine
    return(TRUE)
}

setClass("FLIndex",
    representation(
        name      = "character",
        desc      = "character",
        range     = "numeric",
        distribution = "character",
        index     = "FLQuant",
        index.var = "FLQuant"),
    prototype=prototype(
        name      = character(0),
        desc      = character(0),
        range     = unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0, startf=NA, endf=NA)),
        distribution = character(0),
        index     = new("FLQuant"),
        index.var = new("FLQuant")),
    validity=validFLIndex
)

setValidity("FLIndex", validFLIndex)
remove(validFLIndex)    #   }}}

# class :: FLIndexCom		{{{
setClass("FLIndexCom",
	representation("FLIndex",
		catch="FLQuant",
		catch.wt="FLQuant",
		effort="FLQuant",
		sel.pattern="FLQuant",
		index.q="FLQuant"),
	prototype=prototype(
		name=character(0),
		desc=character(0),
		range=unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0, startf=NA, endf=NA)),
		distribution=character(0),
		index=new("FLQuant"),
		index.var=new("FLQuant"),
		catch=new("FLQuant"),
		catch.wt=new("FLQuant"),
		effort=new("FLQuant"),
		sel.pattern=new("FLQuant"),
		index.q=new("FLQuant"))
)	# }}}

# class :: FLIndexSurvey		{{{
setClass("FLIndexSurvey",
	representation("FLIndex",
		catch.n="FLQuant",
		catch.wt="FLQuant",
		effort="FLQuant",
		index.q="FLQuant"),
	prototype=prototype(
		name=character(0),
		desc=character(0),
		range=unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0, startf=NA, endf=NA)),
		distribution=character(0),
		index=new("FLQuant"),
		index.var=new("FLQuant"),
		catch.n=new("FLQuant"),
		catch.wt=new("FLQuant"),
		effort=new("FLQuant"),
		index.q=new("FLQuant"))
)	# }}}

# class :: FLIndexAcoustic	{{{
setClass("FLIndexAcoustic",
	representation("FLIndex",
		index.q="FLQuant"),
	prototype=prototype(
		name=character(0),
		desc=character(0),
		range=unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0, startf=NA, endf=NA)),
		distribution=character(0),
		index=new("FLQuant"),
		index.var=new("FLQuant"),
		index.q=new("FLQuant"))
)	# }}}

## FLIndex()    {{{
FLIndex <- function(index=FLQuant(), index.var=FLQuant(index), name=character(0), desc=character(0),
	distribution=character(0), startf=NA, endf=NA, plusgroup=NA, class="FLIndex", ...) {

	args <- list(...)
    dims <- dims(index)
	new <- new(class, name = name, desc = desc, distribution = distribution, index = index,
		index.var = index.var, range = unlist(list(min=dims$min, max=dims$max, plusgroup=NA,
		minyear=dims$minyear, maxyear=dims$maxyear, startf=startf, endf=endf)))
	
	for(i in names(args))
		slot(new, i) <- args[[i]]

	return(new)
}   # }}}

## FLIndexCom()		{{{
FLIndexCom <- function(...)
	FLIndex(..., class="FLIndexCom")
# }}}

## FLIndexSurvey()		{{{
FLIndexSurvey <- function(...)
	FLIndex(..., class="FLIndexSurvey")
# }}}

## FLIndexAcoustic()		{{{
FLIndexAcoustic <- function(...)
	FLIndex(..., class="FLIndexAcoustic")
# }}}

## is.FLIndex	{{{
is.FLIndex <- function(x)
    return(inherits(x, "FLIndex"))

is.FLIndexCom <- function(x)
    return(inherits(x, "FLIndexCom"))
is.FLIndexSurvey <- function(x)
    return(inherits(x, "FLIndexSurvey"))
is.FLIndexAcoustic <- function(x)
    return(inherits(x, "FLIndexAcoustic"))
# }}}

## summary :: FLIndex	{{{
setMethod("summary", signature(object="FLIndex"),
    function(object, ...){
        cat(c("An object of class", class(object) ,"\n\n"))
        cat("Name:", object@name, "\n")
        cat("Description:", object@desc, "\n")
        cat("Error Prob. Distr.:", object@distribution, "\n")
        cat("Range:\n")
        print(object@range)
        for (s in names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])) {
           if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
                cat(s, "   \t: EMPTY\n") else
                cat(s, "   \t:[", dim(slot(object,s)),"], \tunits = ", slot(object,s)@units, "\n")
        }
    }
)	# }}}

## dims::FLIndex    {{{
setMethod("dims", signature(obj="FLIndex"),
    # Returns a list with different parameters
    function(obj, ...){
        return(list(
            quant = dim(obj@index)[1],
            min = obj@range[["min"]],
            max = obj@range[["max"]],
            plusgroup = obj@range[["plusgroup"]],
            years = dim(obj@index)[2],
            minyear = obj@range[["minyear"]],
            maxyear = obj@range[["maxyear"]],
            unit = dim(obj@index)[3],
            season = dim(obj@index)[4],
            area = dim(obj@index)[5],
            startf = obj@range[["startf"]],
            endf = obj@range[["endf"]]))
    }
)    # }}}

## window::FLIndex		{{{
setMethod("window", signature(x="FLIndex"),
      function(x, start, end, extend=TRUE, frequency=1) {

        names <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
        
        for (s in names) {
            slot(x, s) <- window(slot(x, s), start=start, end=end,
                extend=extend, frequency=frequency)
            x@range["minyear"] <- start
            x@range["maxyear"] <- end
        }
        return(x)
    }
)	# }}}

## apply::FLIndex	{{{
setMethod("apply", signature(X="FLIndex", MARGIN="list", FUN="function"),
    function(X, MARGIN, FUN, ...) {
    
        for(i in 1:seq(along=MARGIN)) {
            slot(X, MARGIN[[i]]) <- apply(slot(X, MARGIN[[i]]), 1:5, FUN, ...)
        }
        return(X)
    }
)	# }}}

## transform::FLIndex	{{{
setMethod("transform", signature(x="FLIndex"),
    function(x, ...) {

        args <- list(...)

        for (i in 1:seq(along=args)) {
            slot(x, names(args)[i])[,,,,] <- args[[i]][,,,,]
        }
        return(x)
    }
)	# }}}

## as.FLIndex		{{{
if (!isGeneric("as.FLIndex")) {
    setGeneric("as.FLIndex", function(object, ...){
        value  <-  standardGeneric("as.FLIndex")
        value
    })
}	# }}}

## as.FLIndex::FLFleet      {{{
if (!isGeneric("as.FLIndex")) {
    setGeneric("as.FLIndex", function(object, ...){
        value <- standardGeneric("as.FLIndex")
        value
    })
}

setMethod("as.FLIndex", signature(object="FLFleet"),
    function(object, catchname="missing", catchtype="missing", ...) {
    
    # Check if valid fleet
    validObject(object)
    
    # If only one spp in @catch and spp=missing, take it
    if (missing(catchname) && length(object@catches)==1)

    indstock <- 1

    # If spp is character, look for it in @catch
    else if (is.character(catchname)) {
        if(length(object@catches) == 1) {
            if(object@catches[[1]]@name != catchname)
                stop(paste("Catchname ", catchname, "cannot be found in object"))
            else
                indstock <- 1
            } else {

            # get vector of spp in FLCatch
            indstock <- vector()
            for(i in seq(along=object@catches))
                if (object@catches[[i]]@name == catchname) indstock[i] <- i
                    indstock <- indstock[!is.na(indstock)]
                if (length(indstock)==0) stop(paste("Catchname ", catchname,
                    "cannot be found in fleet by as.FLIndex()"))
                if (length(indstock)>0)  stop(paste("More than 1 occurrence of ", catchname,
                    " found in fleet by as.FLIndex()"))
            }

        } else
            stop("stock must be a character string or a number")

        # Output FLIndex object
        # now, indstock contains the catches which need to be put in new Index object.
        # length of indstock==1, so simple copying of slots. If length >1 then calculations are needed

        if (length(indstock)==1){
            fli <- FLIndex(name=paste("catchtype ",
                ifelse((missing(catchtype)||catchtype == "catch"),"catch","landings"),
                "derived from FLfleet, with catchname " , catchname),
                iniFLQuant=object@catches[[indstock]]@landings.n)

            # range (20/5/2005; treated separately because plusgroup is missing in fleet@range
            fli@range["min"] <- object@catches[[indstock]]@range["min"]
            fli@range["max"] <- object@catches[[indstock]]@range["max"]
            fli@range["minyear"] <- object@catches[[indstock]]@range["minyear"]
            fli@range["maxyear"] <- object@catches[[indstock]]@range["maxyear"]
            fli@range["plusgroup"] <- object@catches[[indstock]]@range["max"]

            # q
            fli@q <- object@catches[[indstock]]@q

            # effort, catch
            fli@effort   <-  object@effort
            if (missing(catchtype) || catchtype == "catch") {
                fli@index    <-  object@catches[[indstock]]@catch.n
                fli@index.wt <-  object@catches[[indstock]]@catch.wt
            } else if (catchtype == "landings") {
                fli@index    <-  object@catches[[indstock]]@landings.n
                fli@index.wt <-  object@catches[[indstock]]@landings.wt
            } else
                stop(paste("Catchtype ", catchtype, " not recognized"))
        }
    return(fli)
    }
)   # }}}

## Accesors
invisible(createFLAccesors(new("FLIndex"), exclude='range'))
invisible(createFLAccesors(new("FLIndexCom"), exclude=c('name','desc','range','distribution','index','index.var')))
invisible(createFLAccesors(new("FLIndexSurvey"), exclude=c('name','desc','range','distribution','index','index.var')))
invisible(createFLAccesors(new("FLIndexAcoustic"), exclude=c('name','desc','range','distribution','index','index.var')))
