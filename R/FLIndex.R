# FLIndex.R - FLIndex class and methods

# Author: FLR Team
# Maintainer: Richard Hillary, Imperial College London
# Additions:
# Last Change: 30 mar 2006 16:48
# $Id: FLIndex.R,v 1.20.2.24 2006/04/06 18:57:36 iagoazti Exp $

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
        name         = "character",
        desc         = "character",
        type         = "character",
        range        = "numeric",
        distribution = "character",
        index        = "FLQuant",
        index.var    = "FLQuant",
        catch.n      = "FLQuant",
		catch.wt     = "FLQuant",
		effort       = "FLQuant",
		sel.pattern  = "FLQuant",
		index.q      = "FLQuant"),
    prototype=prototype(
        name         = character(0),
        desc         = character(0),
        type         = character(0),
        range        = unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0, startf=NA, endf=NA)),
        distribution = character(0),
        index        = new("FLQuant"),
        index.var    = new("FLQuant"),
		    catch.n      = new("FLQuant"),
		    catch.wt     = new("FLQuant"),
		    effort       = new("FLQuant"),
		    sel.pattern  = new("FLQuant"),
		    index.q      = new("FLQuant")),
    validity=validFLIndex
)

setValidity("FLIndex", validFLIndex)
remove(validFLIndex)    #   }}}

## FLIndex()    {{{
FLIndex <- function(name=character(0), desc=character(0), distribution=character(0),
    type=character(0), startf=NA, endf=NA, plusgroup=NA, ...) {

	args <- list(...)
	if(length(args)==0)
		args <- list(index=FLQuant())

	dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])

	if(!is.FLQuant(args['index']))
		index <- FLQuant(dimnames=dimnames)

	dims <- dims(index)

	new <- new("FLIndex", name = name, desc = desc, distribution = distribution,
        type=type,
        index = index, index.var = FLQuant(dimnames=dimnames),
        index.q = FLQuant(dimnames=dimnames), sel.pattern = FLQuant(dimnames=dimnames),
        catch.n = FLQuant(dimnames=dimnames), catch.wt = FLQuant(dimnames=dimnames),
        effort = FLQuant(dimnames=dimnames), 
        range = unlist(list(min=dims$min, max=dims$max,
		plusgroup=NA, minyear=dims$minyear, maxyear=dims$maxyear, startf=startf, endf=endf)))
	
	# Add extra arguments
	for(i in names(args)[names(args)!='iniFLQuant'])
		slot(new, i) <- args[[i]]

	# Correctly size other FLQuants.
#    emptyQuants <- names(getSlots(class)[getSlots(class)=="FLQuant"])[
#        !names(getSlots(class)[getSlots(class)=="FLQuant"])%in%c("index", "index.var",
#        names(args))]
#    for(i in emptyQuants)
#        slot(new, i) <- FLQuant(index)
	
	return(new)
}   # }}}

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
		cat("Range:\tmin\tmax\tp+group\tminyear\tmaxyear\tstartf\tendf\n")
		cat("", object@range, "\n", sep="\t")
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
        }
        x@range["minyear"] <- start
        x@range["maxyear"] <- end
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
setMethod("transform", signature(`_data`="FLIndex"),
    function(`_data`, ...) {

        args <- list(...)

        for (i in 1:seq(along=args)) {
            slot(`_data`, names(args)[i]) <- args[[i]]
        }
        return(`_data`)
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

## plot::FLIndex	{{{
setMethod("plot", signature(x="FLIndex",y="missing"),
    function(x, type=c("splom"), ...) {

	    plotinternal <- function(x, ... ) {
        pfun <- function(x,y,...){
          panel.xyplot(x,y, ...)
          if (length(x) > 1) panel.lmline(x,y, lty=1)
        }
        skip <- matrix(1,nrow=dims(x@index)$age-1, ncol=dims(x@index)$age-1)
        xydf <- NULL
        for (age in dims(x@index)$min:(dims(x@index)$max-1) ){
          for (inc in 1:(dims(x@index)$max-age)) {
              years <- dims(x@index)$minyear:(dims(x@index)$maxyear-inc)
              xd <- as.numeric(x@index[as.character(age),as.character(years),])
              yd <- as.numeric(x@index[as.character(age+inc),as.character(years+inc),])
              d <- paste("age",age,"vs",age+inc)
              xydf <- rbind(xydf,cbind(as.data.frame(cbind(xd,yd)),d))
              skip[dims(x@index)$max-dims(x@index)$min+1-inc, age-dims(x@index)$min + 1] <-0
          }
        }
        xydf <- xydf[xydf$xd != 0 & xydf$yd != 0 & !is.na(xydf$xd) & !is.na(xydf$yd),]
        print(xyplot(yd~xd|d,outer=F, col="black", data=xydf, panel=pfun, scales=list(log="e",relation="sliced",draw=FALSE), layout=c(dims(x@index)$age-1,dims(x@index)$age-1), xlab="log index", ylab="log index", skip=as.numeric(skip), main=x@name))
      }
      plotts <- function(x, ...) {
        dps <- as.data.frame(sweep(x@index,1,apply(x@index,  1,mean,na.rm=T),"/"))
        dps$year <- as.numeric(as.character(dps$year))
        dps$age <- as.factor(dps$age)
        print(xyplot(data~year|age,outer=T, type="b", data=dps, scales=list(relation="sliced",draw=TRUE),lty=(5:1),lwd=1.5, ylab="standardised index", ...))
      }

      # The body of the plot method
      validObject(x)
	    type <- type[1]
	    
	    res <- switch(type,
	      "splom" = plotinternal(x=x, ... ),
	      "ts" = plotts(x=x, ... ),
        cat("type must be 'splom' or 'ts' !\n"))
	    # Return result invisibly
	    invisible(res)

    }
)	# }}}

