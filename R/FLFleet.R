# FLFleet - Class for storing catch, discards, and revenue by fleet and species

# Author: FLR Team
# Additions:
# Last Change: 28 Xul 2005 12:27
# $Id: FLFleet.R,v 1.33.2.2 2005/12/19 11:22:22 iagoazti Exp $
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
        if (!dim(t.)[2] == Dim[2]) return("cost, effort and capacity FLQuants must have unequal year dimensions")
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

## validFLCatch                 {{{
validFLCatchYears <- function(object,years) {

    # FLQuants must have same number of years/ages
    Dim <- dim(object@catch.n)
    # and years need to be equal to set years if required
    if (!missing(years)) Dim[2] <- years
    # Moreover, quant has to be equal in all FLQuants
    Quant <- quant(object@catch.n)

    s. <- list("catch.n","catch.wt","catch.sel","landings.n","landings.wt",
        "landings.sel","discards.n","discards.wt","discards.sel","q","price")
    for (i. in s.) {
        t. <- slot(object, i.)
        if (!is.FLQuant(t.)) return (paste(t. ,"is not an FLQuant"))
        if (!all(dim(t.) == Dim)) return(paste("FLQuant dimensions wrong for ", i.," or catch.n"))
	      if (!quant(t.) == Quant) return(paste("quant (first dimension) is wrong for ", i.," or catch.n"))
    }
    
    # except for catch, landings and discards, that have to be dim[1]=1
    Dim[1] <- 1
    s.  <- list("catch", "landings", "discards")
    for (i. in s.) {
        t. <- slot(object, i.)
        if (!is.FLQuant(t.)) return (paste(t. ,"is not an FLQuant"))
        if (!all(dim(t.) == Dim)) return(paste("FLQuant dimensions wrong for ", i.," or catch.n"))
	      if (!quant(t.) == Quant) return(paste("quant (first dimension) is wrong for ", i.," or catch.n"))
    }
    
    # Verify that bounds are correct and correspond to first slot
	  .t  <-getSlots(class(object))
    .t  <-.t[.t=="FLQuant"]
    .t  <-.t[names(.t)!="catch" & names(.t)!="discards" & names(.t)!="landings"]
    if (length(.t)> 0) {
        Par <- dims(.s<-slot(object,names(.t[1])))

        min <- ifelse(is.numeric(object@range["min"]),object@range["min"],NA)
        if (!is.na(min) && (min < Par$min || min > Par$max))
          return("min is outside quant range in FLQuant slots")
	    
	    max <- ifelse(is.numeric(object@range["max"]), object@range["max"], NA)
	    if (!is.na(max) && (max < Par$min || max > Par$max))
	      return("max is outside quant range in FLQuant slots")
	    if (!is.na(min) && !is.na(max) && max < min)
          return("max is lower than min")
        
        plusgroup <- object@range["plusgroup"]
        if (!is.na(plusgroup) && (plusgroup < Par$min || plusgroup > Par$max))
          return("plusgroup is outside [min, max] range in FLQuant slots")
    
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
} # }}}

validFLCatch <- function(object) {
    result <- validFLCatchYears(object)
    return(result)
}

## class::FLCatch               {{{
setClass("FLCatch",
    representation(
        name        = "character",
        gear        = "character",
        range       = "numeric",
        catch       = "FLQuant",
        catch.n     = "FLQuant",
        catch.wt    = "FLQuant",
        catch.sel   = "FLQuant",
        landings    = "FLQuant",
        landings.n  = "FLQuant",
        landings.wt = "FLQuant",
        landings.sel= "FLQuant",
        discards    = "FLQuant",
        discards.n  = "FLQuant",
        discards.wt = "FLQuant",
        discards.sel= "FLQuant",
        q           = "FLQuant",
        price       = "FLQuant"
    ),
    prototype=prototype(
        name        = character(0),
        gear        = character(0),
	    range       = unlist(list(min=NULL, max=NULL, plusgroup=NULL, minyear=NULL, maxyear=NULL)),
        catch       = new("FLQuant"),
        catch.n     = new("FLQuant"),
        catch.wt    = new("FLQuant"),
        catch.sel   = new("FLQuant"),
        landings    = new("FLQuant"),
        landings.n  = new("FLQuant"),
        landings.wt = new("FLQuant"),
        landings.sel= new("FLQuant"),
        discards    = new("FLQuant"),
        discards.n  = new("FLQuant"),
        discards.wt = new("FLQuant"),
        discards.sel= new("FLQuant"),
        q           = new("FLQuant"),
        price       = new("FLQuant")
    ),
	validity=validFLCatch
) # }}}

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
        effort    =new("FLQuant"),
        capacity  =new("FLQuant"),
        crewshare =new("FLQuant"),
        vcost     =new("FLQuant"),
        fcost     =new("FLQuant")),
    validity=validFLFleet
) # }}}

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
)

# window::FLCatch
setMethod("window", signature="FLCatch",
	  function(x, start, end, extend=TRUE, frequency=1) {
        s. <- list("catch.n","catch.wt","catch.sel","catch","landings.n","landings.wt",
           "landings.sel","landings","discards.n","discards.wt","discards.sel","discards","q","price")
         FLC <- FLCatch(name=slot(x, "name"),gear=slot(x, "gear"))
         for (i. in s.) slot(FLC,i.) <-  window(slot(x, i.),start,end)
         return(FLC)
    }
)

#window::FLFleet
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
)

## summary::FLCatch             {{{
setMethod("summary", signature(object="FLCatch"),
    function(object, ...){
	    cat("An object of class \"FLCatch\"\n\n")
		cat("Name:", object@name, "\n")
		cat("Gear:", object@gear, "\n")
	  cat("Range:\tmin\tmax\tminyear\tmaxyear\n")
		cat("", object@range, "\n", sep="\t")
        
		for (s in list("catch", "catch.n", "catch.wt", "catch.sel", "discards",
            "discards.n", "discards.wt", "discards.sel", "landings", "landings.n",
            "landings.wt", "landings.sel", "q", "price")) {
			if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
        }
    }
) # }}}

## FLCatch()                {{{
FLCatch <- function(name, gear, iniFLQuant) {
    if (missing(name))
		   name <- character(0)
    if (missing(gear))
		   gear <- character(0)
    if(missing(iniFLQuant))
        iniFLQuant <- FLQuant()
    if(!is.FLQuant(iniFLQuant))
        stop("FLCatch can only be initialised with a FLQuant object")
    Par <- dims(iniFLQuant)
    iniFLQuant[,,,,]  <- NA
    units(iniFLQuant) <- "NA"
    catch <- new("FLCatch")
    #fill name, stock and gear
    catch@name <- name
    catch@gear <- gear
    
    # fill slots that have first dim > 1
    for(i in list("catch.n","catch.wt","catch.sel","landings.n","landings.wt",
        "landings.sel","discards.n","discards.wt","discards.sel","q","price")) slot(catch, i) <- iniFLQuant

    # slots without 1 dim (quant='all')
    agriniFLQuant <- FLQuant(NA, dimnames=list(quant='all', year=dimnames(iniFLQuant)[[2]], unit=dimnames(iniFLQuant)[[3]],
      season=dimnames(iniFLQuant)[[4]], area=dimnames(iniFLQuant)[[5]]))
    quant(agriniFLQuant) <- quant(iniFLQuant)
            
    for(i in list("catch","landings","discards")) {
        slot(catch, i) <- agriniFLQuant
    }
    slot(catch, "range")  <- unlist(list(min=Par$min, max=Par$max,
                plusgroup=NA, minyear=Par$minyear, maxyear=Par$maxyear))
    return(catch)
} # }}}

## FLFleet()                {{{
FLFleet <- function(name, desc, iniFLQuantFleet, iniFLQuantCatch, catchnames=as.character(1), catchgears=as.character(NULL), ...) {

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
setGeneric("as.FLFleet", function(x) {
        value <- standardGeneric("as.FLFleet")
        value
		}
	)
} # }}}

setMethod("as.FLFleet", signature(x="FLStock"),
    function(x) {
    
        #check arguments
        if (!inherits(x, "FLStock"))
                stop("stock must be an 'FLstock' object!")

        res <- FLFleet(iniFLQuantFleet=x@catch.n[1,], iniFLQuantCatch =x@catch.n,
          desc=paste("created by FLFleet from", x@name), name="Fleet", catchname=x@name, catchgears="NA" )

        res@range <- x@range[c("minyear","maxyear")]

        res@catches[[1]] <- transform(res@catches[[1]], catch.n = x@catch.n, catch.wt = x@catch.wt, catch = x@catch,
          landings.n = x@landings.n, landings.wt = x@landings.wt, landings = x@landings, discards.n = x@discards.n,
          discards.wt = x@discards.wt, discards = x@discards)

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

## transform::FLFLeet
setMethod("transform", signature(x="FLFleet"),
	function(x, ...) {

		args <- list(...)

		for (i in 1:length(args)) {
			slot(x, names(args)[i])[,,,,] <- args[[i]][,,,,]
		}

		return(x)
	}
)

## transform::FLCatch
setMethod("transform", signature(x="FLCatch"),
	function(x, ...) {

		args <- list(...)

		for (i in 1:length(args)) {
			slot(x, names(args)[i])[,,,,] <- args[[i]][,,,,]
		}

		return(x)
	}
)


setMethod("discards", signature(object="FLCatch"),

	function(object) {

#		if (overwrite || !any(complete.cases(slot(object, "discards")))) {
        if(!any(complete.cases(slot(object, "discards.n"))))
            warning("discards.n are needed to estimate total discards")
        if(!any(complete.cases(slot(object, "discards.wt"))))
      	    warning("discards.wt are needed to estimate total discards")

        if(all(dim(slot(object, "discards.n")) == dim(slot(object, "discards.n")))) {
            res <- slot(object, "discards.n")*slot(object, "discards.wt")
            res@.Data[slot(object, "discards.n")@.Data %in% 0] <- 0
            res@.Data[slot(object, "discards.wt")@.Data %in% 0] <- 0
            res <- apply(res, 2:5, sum)
            units(res) <- units(slot(object, "discards.wt"))
				    quant(res) <- quant(slot(object, "discards.wt"))
            return(res)
        }
        else {
            warning("dimensions of discards.n and discards.wt are not the same")
			  }
  }
)

setMethod("landings", signature(object="FLCatch"),

	function(object) {

#		if (overwrite || !any(complete.cases(slot(object, "landings")))) {
        if(!any(complete.cases(slot(object, "landings.n"))))
            warning("landings.n are needed to estimate total landings")
        if(!any(complete.cases(slot(object, "landings.wt"))))
      	    warning("landings.wt are needed to estimate total landings")

        if(all(dim(slot(object, "landings.n")) == dim(slot(object, "landings.n")))) {
            res <- slot(object, "landings.n")*slot(object, "landings.wt")
            res@.Data[slot(object, "landings.n")@.Data %in% 0] <- 0
            res@.Data[slot(object, "landings.wt")@.Data %in% 0] <- 0
            res <- apply(res, 2:5, sum)
            units(res) <- units(slot(object, "landings.wt"))
    				quant(res) <- quant(slot(object, "landings.wt"))
            return(res)
        }
        else {
            warning("dimensions of landings.n and landings.wt are not the same")
			  }
  }
)

setMethod("catch", signature(object="FLCatch"),

	function(object, slot="all", overwrite=FALSE) {

		# catch.n
		if(slot == "n" || slot == "all") {
        if(!any(complete.cases(slot(object, "landings.n"))))
            warning("landings.n are needed to estimate catch.n")
        if(!any(complete.cases(slot(object, "discards.n"))))
            warning("discards.n are needed to estimate catch.n")
        res <- slot(object, "landings.n") + slot(object, "discards.n")
        if(overwrite == TRUE)
            slot(object, "catch.n") <- res
    }

		# catch.wt
		if(slot == "wt" || slot == "all") {
			if(!any(complete.cases(slot(object, "landings.n"))))
          warning("landings.n are needed to estimate catch.wt")
      if(!any(complete.cases(slot(object, "landings.wt"))))
          warning("landings.wt are needed to estimate catch.wt")
      if(!any(complete.cases(slot(object, "discards.n"))))
          warning("discards.n are needed to estimate catch.wt")
      if(!any(complete.cases(slot(object, "discards.wt"))))
          warning("discards.wt are needed to estimate catch.wt")
          
      dis <- slot(object, "discards.n")*slot(object, "discards.wt")
      dis@.Data[slot(object, "discards.n")@.Data %in% 0] <- 0
      dis@.Data[slot(object, "discards.wt")@.Data %in% 0] <- 0

      lan <- slot(object, "landings.n")*slot(object, "landings.wt")
      lan@.Data[slot(object, "landings.n")@.Data %in% 0] <- 0
      lan@.Data[slot(object, "landings.wt")@.Data %in% 0] <- 0

      res <- (dis + lan) /(slot(object, "landings.n") + slot(object, "discards.n"))
			# units
			if (units(slot(object, "discards.wt")) == units(slot(object, "landings.wt")))
				units(res) <- units(slot(object, "discards.wt"))
      if(overwrite == TRUE)
          slot(object, "catch.wt") <- res
		}

		# catch
		if(slot == 'all') {
        if(any(is.na(slot(object, "landings.n"))))
            warning("There are NAs present in landings.n")
        if(any(is.na(slot(object, "landings.wt"))))
            warning("There are NAs present in landings.wt")
        if(any(is.na(slot(object, "discards.n"))))
            warning("There are NAs present in discards.n")
        if(any(is.na(slot(object, "discards.wt"))))
            warning("There are NAs present in discards.wt")
        res <- landings(object) + discards(object)
			# units
			if (units(slot(object, "catch.wt")) != "NA")
				units(res) <- units(slot(object, "discards.wt"))
			else if (units(slot(object, "discards.wt")) == units(slot(object, "landings.wt")))
				units(res) <- units(slot(object, "discards.wt"))

		  # quant
		  quant(res) <- quant(slot(object, "landings.n"))

      if(overwrite==TRUE) {
        slot(object, "catch") <- res
        return(object)
      }
    }
    return(res)
	}
)

## summary::FLFLeet
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
)


## plot (make specific plots to display units graphically)
setMethod("plot", signature(x="FLCatch", y="missing"),
	function(x, y, type=c("summary", "composite", "catch", "Wt", "WtRes"),
		quants=1:dim(x@catch.n)[1], unit=1, season=1, area=1,
		f=0.5, scale=1, pch=19, cols=c("red", "black", "blue"), xlab="", ylab="", main="", ...){

	    plotSummary  <- function(x, unit, season, area) {

		      def.par <- par(no.readonly = TRUE)# save default, for resetting...
		      par(mfrow=c(2,2))

	  	    addlines <- function(dat, range) lines(range,dat)

		      dimnames. <- dimnames(x@catch.n)
		      quant. <- quant(x@discards.n)
	   	    la.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			      quant=dimnames.[[quant.]],y=as.vector(x@landings.n))
		      lw.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			      quant=dimnames.[[quant.]],y=as.vector(x@landings.wt))
		      dw.<-cbind(year=as.numeric(rep(dimnames.$year,each=length(dimnames.[[quant.]]))),
			      quant=dimnames.[[quant.]],y=as.vector(x@discards.wt))

		      yrange <- as.character(x@range["minyear"]:x@range["maxyear"])
		      arange <- as.character(x@range["min"]:x@range["max"])

		      ## LANDINGS
		      if (any(complete.cases(x@landings))){
		          barplot(as.vector(x@landings),col="white", ylab=attributes(x@landings)$units, names.arg=yrange)
          } else suppressWarnings(barplot(rep(0, length=length(yrange)), ylab=attributes(x@landings)$units, ylim=c(0,1),names.arg=yrange))
          title(main= "Landings")

		      ## DISCARDS WEIGHTS at quant
		      if (any(complete.cases(x@discards.wt))){
		          suppressWarnings(plot(yrange,x@discards.wt[arange[1],yrange,,,], type="null",
			          ylab=attributes(x@discards.wt)$units, xlab="", ylim=c(0,max(x@discards.wt,na.rm=T))))
		          tapply(dw.[,3],dw.[,2],addlines,yrange)
          } else suppressWarnings(plot(yrange,yrange, type="n", ylab=attributes(x@discards.wt)$units, xlab="", ylim=c(0,1)))
          title(main=paste("Discards Weight at",quant.))

		      ## LANDINGS NUMBERs AT Quant
		      if (any(complete.cases(x@landings.n))){
		          suppressWarnings(plot(yrange,x@landings.n[arange[1],yrange,,,],type="null", xlab="", ylab=quant.,
				        ylim=c(min(as.numeric(arange)),max(as.numeric(arange)))))
		          symbols(la.[,1],la.[,2],circles=la.[,3], inches=0.1, add=T)
		      } else suppressWarnings(plot(yrange,yrange, type="n", ylab=quant., xlab="", ylim=c(min(as.numeric(arange)),max(as.numeric(arange)))))
          title(main=paste("Landings numbers at ",quant.))

		      ## LANDINGS WEIGHTS
		      if (any(complete.cases(x@landings.wt))){
		          suppressWarnings(plot(yrange,x@landings.wt[arange[1],yrange,,,], type="null", ylab=attributes(x@landings.wt)$units, xlab="",
		            ylim=c(0,max(x@landings.wt))))
              tapply(lw.[,3],lw.[,2],addlines,yrange)
		       } else suppressWarnings(plot(yrange,yrange, type="n", ylab=attributes(x@landings.wt)$units, xlab="", ylim=c(0,1)))
           title(main=paste("Landings weight at ",quant.))
	    }

      plotComposite <- function(x, unit, season, area, xlab, ylab, main, ...){

          def.par <- par(no.readonly = TRUE)# save default, for resetting...

          # Extract a submatrix, and set up various variables
          if (sum(x@catch.n, na.rm=TRUE) == 0) stop("This object contains no catch.n data.")
          if (sum(x@catch.wt,na.rm=TRUE) == 0) stop("This object contains no catch.wt data")
          if (sum(x@catch,   na.rm=TRUE) == 0) stop("This object contains no catch data")

          if (xlab == "") xlab <- "Year"
			    if (ylab == "") ylab <- c("Catch", "SOP", paste("Catch-at-",quant(x@catch.n),sep="" ))
			    if (main == "") main <- paste(x@name, "catches\n")
			    Years    <- as.numeric(dimnames(x@catch)$year)
			    Catch    <- catch(x)[,,unit[1], season[1], area[1]]

			    # Set up layout
			    nf <- layout(matrix(c(1:3), 3, 1, byrow=TRUE), widths=7,
				  heights=c(2.5, 0.75, 4), respect=TRUE)

			    # Plot total catch
			    par(mar=c(0, 5, 5, 5), lab=c(5, 3, 7))
			    plot(Years, Catch, ylab=ylab[1], xaxt="n", main=main, col=cols[2], type="l")
			    points(Years, x@catch[,,unit[1], season[1], area[1]], col=cols[1])
			    axis(3)

          # Plot differences between measured and calculated yields
          par(mar=c(0, 5, 0, 5), lab=c(5, 3, 7))
	        plot(Years,(x@catch[,,unit[1], season[1], area[1]] - Catch)/ Catch,
		      type="h", ylab=ylab[2], xaxt="n", yaxt="n", col=cols[2])
	        abline(h=0)
	        axis(4)

			    # Plot catch-at-quant
			    par(mar=c(5, 5, 0, 5), lab=c(5, 3, 7))
          y <- cbind(as.vector(x@catch.n), rep(as.numeric(dimnames(x@catch.n)$year),
				    each=dim(x@catch.n)[1]), as.numeric(dimnames(x@catch.n)[[quant(x@catch.n)]]))
			    y[,1] <- y[,1]/rep(tapply(y[,1], y[,2], sum, na.rm=TRUE), each=length(unique(y[,3])))
			    plot(y[,2], y[,3], cex=y[,1] * length(unique(y[,3])) / 2.5, ylab=ylab[3],	xlab=xlab, col=cols[1])

			    par(def.par)

          # Return nothing
			    invisible(NULL)
	   }
	    
		 plotCatch <- function(x, unit, season, area, cols, xlab, ylab, main, ...){

         if (sum(x@catch.n, na.rm=TRUE) == 0) stop("This object contains no catch.n data")

         if (xlab == "") xlab <- "Year"
			   if (ylab == "") ylab <- "log(catch.n a-1,t-1/catch.n a,t)"
			   if (main == "") main <- paste(x@name, "Log catch ratios")

         Nrow <- length(as.integer(dimnames(x@catch.n)[[1]]))
			   Ncol <- length(as.integer(dimnames(x@catch.n)[[2]]))

			   t1 <- x@catch.n[1:Nrow-1, 1:Ncol-1, unit, season, area]
			   t2 <- x@catch.n[2:Nrow, 2:Ncol, unit, season, area]
			   tt <- log(t1/t2)

			   tt <- cbind(catch=as.vector(tt), year=sort(rep(as.integer(dimnames(tt)[[2]]),
					length(as.integer(dimnames(tt)[[1]])))),	age=rep(as.integer(dimnames(tt)[[1]]),
					length(as.integer(dimnames(tt)[[2]]))))
			   tt <- cbind(tt, yrcls=(tt[,2]-tt[,3]))
			   tt <- tt[order(tt[, 4]), ]

			   plot(tt[,2], tt[,1], xlab=xlab, ylab=ylab, main=main, col=cols[1], ...)
			   for (i in range(tt[,4])[1]:range(tt[,4])[2]) lines(tt[tt[, 4] == i, 2], tt[tt[, 4] == i, 1], col=cols[2], ...)
			   invisible(tt)
		 }

		 plotWt <- function(x, quants, unit, season, area, f, xlab, ylab, main, ...){

         opar <- par(no.readonly = TRUE)
			   on.exit(par(opar))
			   par(mfrow=c(1,1))

			   if (sum(x@catch.wt, na.rm=TRUE) == 0)	stop("No catch weight data (catch.wt)")
				
			   if (xlab == "") xlab <- "Year"
			   if (ylab == "") ylab <- paste("catch weight-at-",quant(x@catch.wt),sep="")
			   if (main == "") main <- paste("catch weight", x@name, ":", quant(x@catch.wt), quants[1], "to", quants[length(quants)])

         plot(as.integer(dimnames(x@catch.wt)$year), x@catch.wt[quants[1],,unit,season,area],
				   ylim=c(0, max(x@catch.wt[quants,,unit,season,area], na.rm=TRUE) * 1.1),
			     xlab=xlab, ylab=ylab, main=main, col=cols[1])

         sub <- x@catch.wt[quants[1],,unit,season,area]
         good <- complete.cases(sub)
         lines(lowess(as.integer(dimnames(sub)$year[good]), as.vector(sub)[good], f=f), col=cols[2])

         for (i in quants[-1]){
				     points(as.integer(dimnames(x@catch.wt)$year), x@catch.wt[i,,unit,season,area],	   col=cols[1])
             sub <- x@catch.wt[i,,unit,season,area]
             good <- complete.cases(sub)
             if (any(good)) lines(lowess(as.integer(dimnames(sub)$year[good]), as.vector(sub)[good], f=f), col=cols[2])
			   }
			   # Return nothing
			   invisible(NULL)
		 }

		 plotWtRes <- function(x, quants, unit, season, area, f, scale, pch, cols, xlab, ylab, main, ...){

         opar <- par(no.readonly = TRUE)
			   on.exit(par(opar))
			   par(mfrow=c(1,1))

			   if (sum(x@catch.wt, na.rm=TRUE) == 0) stop("No catch weight data (catch.wt)")

         if (length(cols) < 2) cols <- rep(cols, 2)
			   if (xlab == "") xlab <- "Year"
			   if (ylab == "") ylab <- quant(x@catch.wt)
			   if (main == "") main <- paste("Weight residuals", x@name, ":", quant(x@catch.wt), quants[1], "to", quants[length(quants)])

         #get data and summaries etc.
			   wt <- x@catch.wt[quants,,unit,season,area,drop=TRUE] / matrix(apply(x@catch.wt[quants,
		     ,unit,season,area], 1, mean, na.rm=T), length(quants), length(x@catch.wt[1,,unit,season,area]))
			   X <- as.integer(names(wt[1,]))
			   res <- wt

			   #smooth and get residuals
         for (i in quants){
             wti <-wt[i,]
             good <- complete.cases(wti)
             if (any(good)){
                 resi <-as.numeric(rep(NA,length=length(wti)))
                 resi[good] <- log(wti[good] / lowess(X[good], wti[good], f=f)$y)
                 res[i,] <- resi
             }
         }

         Res <- cbind(as.vector(res), as.integer(rep(dimnames(res)[[1]], dim(res)[2])),
		 	     as.integer(rep(dimnames(res)[[2]], each=dim(res)[1])))
			   flag <- Res[,1] > 0
			   plot(Res[flag,3], Res[flag,2], pch=pch, cex=Res[flag,1] * 10 * scale, col=cols[1], xlim=c(min(Res[,3]),max(Res[,3])),xlab=xlab, ylab=ylab, main=main)
			   points(Res[!flag,3], Res[!flag,2], pch=pch,cex=-Res[!flag,1] * 10 * scale, col=cols[2])

         # Return results
			   invisible(list(hat=wt, residuals=res))
		 }


		 # The body of the plot method
     validObject(x)
     type <- type[1]
		 res <- switch(type,
	     "summary" = plotSummary(x=x, unit=unit, season=season, area=area),
	     "composite" = plotComposite(x=x, unit=unit,  season=season, area=area, xlab=xlab, ylab=ylab, main=main,...),
			 "catch" = plotCatch(x=x, unit=unit, season=season, area=area, cols=cols, xlab=xlab, ylab=ylab, main=main, ...),
			 "Wt" = plotWt(x=x, quants=quants, unit=unit, season=season, area=area,	f=f, xlab=xlab, ylab=ylab, main=main, ...),
			 "WtRes" = plotWtRes(x=x, quants=quants, unit=unit, season=season, area=area, f=f, scale=scale, pch=pch, cols=cols,
         xlab=xlab, ylab=ylab, main=main, ...),
			 cat("type must be 'summary', 'composite', 'Wt', 'WtRes', 'catch' or 'ts'!\n"))
		 # Return result invisibly
		 invisible(res)
	}
)

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




