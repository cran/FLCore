# FLFleet - Class for storing catch, discards, and revenue by fleet and species

# Author: FLR Team
# Maintainer: Jan Jaap Poos, RIVO
# Additions:
# Last Change: 30 mar 2006 22:31
# $Id: FLFleet.R,v 1.33.2.8 2006/04/26 16:08:52 ejardim Exp $
# to do: in creator of FLCatch, make possibility to give ranges?

# show for FLCatch should not show the quants that have all NA's in
# see also TO DO's in code

## validFLFleet                 {{{

validFLFleet <- function(object){

    # FLQuants must have same number of years/ages, therefor take dimensions of one FLQuant
    Dim <- dim(object@crewshare)
    #check if all quants are equal
    Quant <- quant(object@crewshare)
    
    s. <- list("vcost","fcost","crewshare","effort","capacity")
    for (i. in s.) {
        t. <- slot(object, i.)
        if (!is.FLQuant(t.)) return (paste(t. ,"is not an FLQuant"))
        if (!dim(t.)[2] == Dim[2]) return("cost, effort and capacity FLQuants must have equal year dimensions")
        if (!quant(t.) == Quant) return(paste("quant (first dimension) is wrong for ", i.," or catch.n"))
    }
    # check catch list contains FLCatch objects and these are valid
    for (i in 1:length(slot(object,"catches"))) {
        if(!class(slot(object, "catches")[[i]])=="FLCatch")
          return("All objects in 'catches' should be of type FLCatch")
        result <- validFLCatchYears(slot(object, "catches")[[i]], dim(object@crewshare)[2])
          if (!result == TRUE) return(result)
    }
    #nothing wrong, return true
    return(TRUE)
} # }}}

## class::FLFleet               {{{
setClass("FLFleet",
    representation(
        name      ="character",
        desc      ="character",
        range     ="numeric",
        catches   ="list",
        effort    ="FLQuant",
        capacity  ="FLQuant",
        crewshare ="FLQuant",
        vcost     ="FLQuant",
        fcost     ="FLQuant"),
    prototype=prototype(
        name      =character(0),
        desc      =character(0),
        range     =as.numeric(unlist(list(minyear=NULL, maxyear=NULL))),
        catches   =list(),
        effort    =FLQuant(),
        capacity  =FLQuant(),
        crewshare =FLQuant(),
        vcost     =FLQuant(),
        fcost     =FLQuant()),
    validity=validFLFleet
)
invisible(createFLAccesors(new("FLFleet"), exclude=c('range', 'catches')))	# }}}

## revenue              {{{
if (!isGeneric("revenue")) {
setGeneric("revenue", function(object,...){
        value <- standardGeneric("revenue")
        value
})}

# revenue::FLFleet
setMethod("revenue", signature(object="FLFleet"),
    function(object, type="gross", ...) {
        validObject(object)
        grossrevenue = FLQuant(0,dim=dim(object@catches[[1]]@landings),dimnames=dimnames(object@catches[[1]]@landings))
        for (i in 1:length(slot(object,"catches"))) {
            validObject(slot(object, "catches")[[i]])
            grossrevenue=  grossrevenue +  apply((object@catches[[i]]@landings.n * object@catches[[i]]@landings.wt * object@catches[[i]]@price),2:5,sum)
        }
        if (type=="gross") return(grossrevenue)
        else {     #return nettrevenue
           if (!all(complete.cases(apply(object@fcost,2,sum) + apply(object@vcost,2,sum) + apply(object@crewshare,2,sum)))) print("One of the cost/crewshare slots contains missing data")
           else return(grossrevenue - (apply(object@fcost,2:5,sum) + apply(object@vcost,2:5,sum) + apply(object@crewshare,2:5,sum)))
        }
    }
)   # }}}

## window    {{{
setMethod("window", signature="FLFleet",
	  function(x, start, end, extend=TRUE, frequency=1) {
         FLF <- FLFleet()

         #window slots existing of FLquant
         s. <- list("vcost","fcost","crewshare","effort","capacity")
         for (i. in s.) slot(FLF,i.) <-  window(slot(x, i.),start,end)

         #window catches slot
         for (i. in 1:length(slot(x,"catches"))) {
            slot(FLF,"catches")[[i.]] <-  window(slot(x,"catches")[[i.]],start,end)
         }
         return(FLF)
    }
)   # }}}

## FLFleet()                {{{
FLFleet <- function(name, desc, iniFLQuantFleet, iniFLQuantCatch,
    catchnames=as.character(1), catchgears=as.character(NULL), ...) {

    #general checks, creation of quants if none given as argument to constructor
    if (missing(name))
		   name <- character(0)
    if (missing(desc))
		   desc <- character(0)
    if(missing(iniFLQuantCatch))
        iniFLQuantCatch <- FLQuant()
	  else{
    	if(!is.FLQuant(iniFLQuantCatch) && !is.list(iniFLQuantCatch))
        	stop("FLCatch in FLFleet can only be initialised with a FLQuant object or a list of FLQuant objects of the same length of 'catchnames'")
		  if(is.list(iniFLQuantCatch) && length(iniFLQuantCatch) != length(catchnames))
		    stop("FLCatch in FLFleet can only be initialised with a FLQuant object or a list of FLQuant objects of the same length of 'catchnames'")
	  }

    if(class(iniFLQuantCatch) == "list") cyears <- range(sapply(sapply(iniFLQuantCatch, dimnames)[2,], as.numeric))
	  else cyears <- range(as.numeric(dimnames(iniFLQuantCatch)[[2]]))

    if(missing(iniFLQuantFleet))
    	iniFLQuantFleet <- FLQuant(dimnames = list(quant = "all", year = cyears[1]:cyears[2],	unit = "unique", season = "all", area = "unique"))
    if(!is.FLQuant(iniFLQuantFleet)) stop("FLFleet can only be initialised with a FLQuant object")

    # check if the range of years for fleet span range of years in catches
    fyears <-  range(as.numeric(dimnames(iniFLQuantFleet)$year))

    if((fyears[1] > cyears[1]) || (fyears[2] < cyears[2]))
			  stop("year range in 'iniFLQuantFleet' must cover year ranges in 'iniFLQuantCatch'")

    #create the capacity, effort and costs of fleet
    Par <- dims(iniFLQuantFleet)
    iniFLQuantFleet[,,,,]  <- NA
    units(iniFLQuantFleet) <- "NA"
    fleet <- new("FLFleet")
    fleet@name <- name
    fleet@desc  <- desc
    slots <- list("fcost","vcost","crewshare","effort","capacity")
    for(i in slots) slot(fleet, i) <- iniFLQuantFleet

    # create catches
    # first make the stock and gear list of equal length to catchnames
    catchgearsrecycled  <- rep(catchgears, length.out=length(catchnames))

    # create a number list of quants that can be used to create catches within fleet, equal to the number of catchnames,
	  # if the argument to constructor is already list, then no action
    iniFLQuantCatchrecycled <- list()
    if(!is.list(iniFLQuantCatch)){
        for(i in 1:length(catchnames)) iniFLQuantCatchrecycled[[i]] <- iniFLQuantCatch
    }
    else iniFLQuantCatchrecycled <- iniFLQuantCatch

    for(i in 1:length(catchnames)) {
        slot(fleet,"catches")[[catchnames[[i]]]] <- FLCatch(catchnames[[i]],
			    catchgearsrecycled[[i]], window(iniFLQuantCatchrecycled[[i]],fyears[1],fyears[2]))
    }
    slot(fleet, "range")  <- unlist(list(minyear=fyears[1], maxyear=fyears[2]))

    return(fleet)
}
# }}}

## is.FLFleet               {{{
is.FLFleet <- function(x)
	return(inherits(x, "FLFleet"))
# }}}

## as.FLFleet               {{{
if (!isGeneric("as.FLFleet")) {
setGeneric("as.FLFleet", function(object) {
        value <- standardGeneric("as.FLFleet")
        value
		}
	)
}
setMethod("as.FLFleet", signature(object="FLStock"),
    function(object) {
        res <- FLFleet(iniFLQuantFleet=object@catch.n[1,], iniFLQuantCatch =object@catch.n,
            desc=paste("created by FLFleet from", object@name), name="Fleet",
            catchname=object@name, catchgears="NA" )

        res@range <- object@range[c("minyear","maxyear")]

        res@catches[[1]] <- transform(res@catches[[1]], catch.n = object@catch.n,
        catch.wt = object@catch.wt, catch = object@catch, landings.n = object@landings.n,
        landings.wt = object@landings.wt, landings = object@landings,
        discards.n = object@discards.n, discards.wt = object@discards.wt,
        discards = object@discards)

        if (!validObject(res))
            stop("FLFleet is not valid!")
        return(res)
    }
) # }}}

## as.FLStock::FLFleet      {{{
setMethod("as.FLStock", signature(object="FLFleet"),
    function(object, name="missing", gear="missing", ...) {
        # Check if valid fleet
        validObject(object)

        # check if all missing
        if (missing(name) && missing(gear) && length(object@catches)==1) indstock <- 1
        if (missing(name) && missing(gear) && length(object@catches)>1 ) stop(paste("multiple catches in fleet, but no name or gear argument to function"))
        # get vector of spp in FLCatch
        indstock <- vector()
        if (is.character(name) && missing(gear)){
            for(i in 1:length(object@catches)) if (object@catches[[i]]@name == name) indstock[i] <- i
        }
        else if (missing(name) && is.character(gear)){
            for(i in 1:length(object@catches)) if (object@catches[[i]]@gear == gear) indstock[i] <- i
        }
        else if (is.character(name) && is.character(gear)){
            for(i in 1:length(object@catches)) if ((object@catches[[i]]@name == name) && (object@catches[[i]]@gear == gear)) indstock[i] <- i
        }
        else stop("name and gear must be a character string or a number")
        indstock <- indstock[!is.na(indstock)]
        if (length(indstock)==0) stop(paste("no catches present in fleet with name and gear satisfying argument to as.FLStock()"))

        # Output stock object
        # indstock contains the catches which need to be put in new stock object.
        # if length of indstock==1 then simple copying of slots. If length >1 then calculations are needed
        if (length(indstock)==1){
            fls <- FLStock(name=paste("catch slot derived from FLfleet, with name", name, "and gear", gear ), iniFLQuant=object@catches[[indstock]]@landings.n)

            # catch, discards and landings
            for (sl in c("range", "catch", "catch.n", "catch.wt", "landings",
                "landings.n", "landings.wt", "discards", "discards.n",
                "discards.wt")) {
                    slot(fls, sl) <- slot(object@catches[[indstock]], sl)
            }
        } else { #length of indstock larger than one meaning two catches slots have to be combinedS
            fls <- FLStock(name=paste("combinaton of catch slots derived from FLfleet, with name" , name, "and gear", gear ), iniFLQuant=object@catches[[ indstock[1] ]]@landings.n)
            # first catch can be put in  first slot, weights are multiplied by numbers for weighted averaging
            for (sl in c( "catch", "catch.n", "landings", "landings.n", "discards", "discards.n")) {
                      slot(fls, sl) <- slot(object@catches[[indstock[1]]], sl)
            }
            slot(fls, "catch.wt")    <- slot(object@catches[[indstock[1]]], "catch.wt")    * slot(object@catches[[indstock[1]]], "catch.n")
            slot(fls, "landings.wt") <- slot(object@catches[[indstock[1]]], "landings.wt") * slot(object@catches[[indstock[1]]], "landings.n")
            slot(fls, "discards.wt") <- slot(object@catches[[indstock[1]]], "discards.wt") * slot(object@catches[[indstock[1]]], "discards.n")

            # all other catch slots are either added, or product of weight and numers added
            for (i in 2:length(indstock)){
                for (sl in c("catch", "catch.n", "landings", "landings.n", "discards", "discards.n")) {
                      slot(fls, sl) <- slot(fls,sl) + slot(object@catches[[indstock[i]]], sl)
                }
                slot(fls, "catch.wt")    <- slot(fls,"catch.wt")    + slot(object@catches[[indstock[i]]], "catch.wt")    * slot(object@catches[[indstock[i]]], "catch.n")
                slot(fls, "landings.wt") <- slot(fls,"landings.wt") + slot(object@catches[[indstock[i]]], "landings.wt") * slot(object@catches[[indstock[i]]], "landings.n")
                slot(fls, "discards.wt") <- slot(fls,"discards.wt") + slot(object@catches[[indstock[i]]], "discards.wt") * slot(object@catches[[indstock[i]]], "discards.n")
            }
            # if done adding, complete weighted averaging by dividing weights by numbers
            slot(fls, "catch.wt")    <- slot(fls,"catch.wt")    / slot(fls,"catch.n")
            slot(fls, "landings.wt") <- slot(fls,"landings.wt") / slot(fls, "landings.n")
            slot(fls, "discards.wt") <- slot(fls,"discards.wt") / slot(fls, "discards.n")
        }
        return(fls)
   }
) # }}}

## update::FLStock, FLFLeets

## update::FLStock, FLFLeet

## transform::FLFLeet {{{
setMethod("transform", signature(`_data`="FLFleet"),
	function(`_data`, ...) {

		args <- list(...)

		for (i in 1:length(args)) {
			slot(`_data`, names(args)[i]) <- args[[i]]
		}

		return(`_data`)
	}
) #}}}

## summary::FLFLeet     {{{
setMethod("summary", signature(object="FLFleet"),
	function(object, ...){
	    cat("An object of class \"FLFleet\"\n\n")
	    cat("Name:", object@name, "\n")
	    cat("Range:\tminyear\tmaxyear\n")
	    cat("", object@range, "\n", sep="\t")
	    cat("Description:", object@desc, "\n")
	    cat("Quant:", quant(object@fcost), "\n\n")

		  for (s in list("vcost", "fcost", "crewshare","effort","capacity")) {
			if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
		  }
      cat("Catches  \t:", length(slot(object, "catches")), "Catches \n")
      for (l in 1:length(slot(object, "catches"))) {
	  	# TODO output only brief summary of FLCatches
          summary((slot(object, "catches"))[[l]])
		  }
 	}
)   # }}}

## plot (make specific plots to display units graphically)
setMethod("plot", signature(x="FLFleet", y="missing"),
	function(x, y, type=c("summary"), quant=1:dim(x@effort)[1], unit=1, season=1, area=1,
		f=0.5, scale=1, pch=19, cols=c("red", "black", "blue"), xlab="", ylab="", main="", ...){

	    plotSummary  <- function(x, unit, season, area) {

		      def.par <- par(no.readonly = TRUE)# save default, for resetting...
		      par(mfrow=c(2,2))

	  	    addlines <- function(dat, range) lines(range,dat)

          dimnames. <- dimnames(x@effort)
		      quant. <- quant(x@effort)
	   	    le.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			      quant=dimnames.[[quant.]],y=as.vector(x@effort))
		      lca.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			      quant=dimnames.[[quant.]],y=as.vector(x@capacity))
		      dco.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			    quant=dimnames.[[quant.]],y=as.vector(x@fcost + x@vcost + x@crewshare))

		      yrange <- as.character(x@range["minyear"]:x@range["maxyear"])
		      #arange <- as.character(x@range["min"]:x@range["max"])

          ## EFFORT
          if (any(complete.cases(x@effort))){
		          suppressWarnings(plot(yrange,x@effort[1,yrange,,,], type="null",
			          ylab=attributes(x@effort)$units, xlab="", ylim=c(0,max(x@effort,na.rm=T))))
		          tapply(le.[,3],le.[,2],addlines,yrange)
		      } else suppressWarnings(plot(yrange,yrange, type="n", ylab=attributes(x@effort)$units, xlab="", ylim=c(0,1)))
		      title(main="Effort")

          ## CAPACITY
          if (any(complete.cases(x@capacity))){
		          suppressWarnings(plot(yrange,x@capacity[1,yrange,,,], type="null",
			          ylab=attributes(x@capacity)$units, xlab="", ylim=c(0,max(x@capacity,na.rm=T))))
		          tapply(lca.[,3],le.[,2],addlines,yrange)
          } else suppressWarnings(plot(yrange,yrange, type="n", ylab=attributes(x@capacity)$units, xlab="", ylim=c(0,1)))
          title(main="Capacity")

		      ## COSTS
          if (any(complete.cases(x@fcost + x@vcost + x@crewshare))){
            suppressWarnings(plot(yrange,(x@fcost + x@vcost + x@crewshare)[1,yrange,,,], type="null",
			        ylab=attributes(x@vcost)$units, xlab="", ylim=c(0,max((x@fcost + x@vcost + x@crewshare),na.rm=T))))
		        tapply(dco.[,3],dco.[,2],addlines,yrange)
		      } else suppressWarnings(plot(yrange,yrange, type="n", ylab=attributes(x@vcost)$units, xlab="", ylim=c(0,1)))
		      title(main="Total Costs")

		      ## LANDINGS FOR ALL Catches
		      # find max in landings of all catch objects within fleet
		      maxy. <- 0
		      for(i in 1:length(x@catches)) suppressWarnings(maxy. <- ifelse(max(x@catches[[i]]@landings,na.rm=T) > maxy., max(x@catches[[i]]@landings), maxy.))
		      #plot landings
		      suppressWarnings(plot(yrange,x@catches[[1]]@landings[,yrange,,,], type="null",
			      ylab=attributes(x@catches[[1]]@catch)$units, xlab="", ylim=c(0,maxy.)))
		      for(i in 1:length(x@catches)) lines(yrange,as.vector(x@catches[[i]]@landings[,yrange,,,]))
		      title(main="Landings by stock/gear")
	    }

	    # The body of the plot method
	    validObject(x)
	    type <- type[1]
	    res <- switch(type,
	      "summary" = plotSummary(x=x, unit=unit, season=season, area=area),
		    cat("type must be 'summary'!\n"))
	    # Return result invisibly
	    invisible(res)
	}
)

## metier()
# FLFleet  <-  metier(FLFLeet, range, spp, area, unit, season)


## catches()
# FLCatch  <- catches(FLFleet, name, gear)
# FLCatches <- catches(FLFleet)

## FLFleet accesors
## FLCatches class
