# FLStock.R - FLStock class and methods

# Author: FLR Team
# Additions:
# Last Change: 19 Dec 2005 13:12
# $Id: FLStock.R,v 1.42.2.3 2005/12/20 08:16:00 iagoazti Exp $

# Reference:
# Notes:

## class :: FLStock			{{{
validFLStock <- function(object) {

	## FLQuant objects that must have same dimensions
	dim <- dim(object@catch.n)

	s <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n", "landings.wt",
		"stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn", "m.spwn")
	for (i in s) {
		# dim
		if (is.FLQuant(slot(object, i)) & !all(dim(slot(object, i)) == dim))
			stop(paste("FLQuant dimensions wrong for", i))
		par <- dims(slot(object, i[[1]]))
	
		# quant range
		min <- ifelse(is.numeric(object@range["min"]), object@range["min"], NA)
		if (!is.na(min) && (min < par$min || min > par$max))
			stop("min is outside quant range in FLQuant slots")
		max <- ifelse(is.numeric(object@range["max"]), object@range["max"], NA)
		if (!is.na(max) && (max < par$min || max > par$max))
			stop("max is outside quant range in FLQuant slots")
		if (!is.na(min) && !is.na(max) && max < min)
			stop("max is lower than min")
		plusgroup <- object@range["plusgroup"]
		if (!is.na(plusgroup) && (plusgroup < par$min || plusgroup > par$max))
			stop("plusgroup is outside [min, max] range in FLQuant slots")

		# year range
		minyear <- object@range["minyear"]
		if (!is.na(minyear) && (minyear < par$minyear || minyear > par$maxyear))
			stop("minyear is outside years range in FLQuant slots")
		maxyear <- object@range["maxyear"]
		if (!is.na(maxyear) && (maxyear < par$minyear || maxyear > par$maxyear))
			stop("maxyear is outside years range in FLQuant slots")
		if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
			stop("maxyear is lower than minyear")

	}

	# For catch, landings and stock, first dim (age/length) is always 'all'
	dim[1] <- 1
	
	s  <- list("catch", "landings", "discards")

	for (i in s) {
		if (is.FLQuant(slot(object, i)) & !all(dim(slot(object, i)) == dim))
		return(paste("FLQuant dimensions wrong for", i))
	}

	# Everything is fine
	return(TRUE)
}

setClass("FLStock",
	representation(
	name		="character",
	desc		="character",
	range       ="numeric",
	catch	    ="FLQuant",
	catch.n	    ="FLQuant",
	catch.wt	="FLQuant",
	discards	="FLQuant",
	discards.n  ="FLQuant",
	discards.wt ="FLQuant",
	landings	="FLQuant",
	landings.n  ="FLQuant",
	landings.wt ="FLQuant",
	stock	    ="FLQuant",
	stock.n	    ="FLQuant",
	stock.wt	="FLQuant",
	m		    ="FLQuant",
	mat		    ="FLQuant",
	harvest	    ="FLQuant",
	harvest.spwn="FLQuant",
	m.spwn	    ="FLQuant"
	),
	prototype=prototype(
		name	=character(0),
		desc	=character(0),
		range	=unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0)),
		catch	=FLQuant(array(NA, dim=c(1,1,1,1,1),
			dimnames=list('all', year=0, unit='unique', season='all', area='unique'))),
		catch.n	=FLQuant(),
		catch.wt=FLQuant(),
		discards=FLQuant(array(NA, dim=c(1,1,1,1,1),
			dimnames=list('all', year=0, unit='unique', season='all', area='unique'))),
		discards.n =FLQuant(),
		discards.wt=FLQuant(),
		landings   =FLQuant(array(NA, dim=c(1,1,1,1,1),
			dimnames=list('all', year=0, unit='unique', season='all', area='unique'))),
		landings.n =FLQuant(),
		landings.wt=FLQuant(),
		stock	   =FLQuant(array(NA, dim=c(1,1,1,1,1),
			dimnames=list('all', year=0, unit='unique', season='all', area='unique'))),
		stock.n	 =FLQuant(),
		stock.wt =FLQuant(),
		m		 =FLQuant(),
		mat		 =FLQuant(),
		harvest	 =FLQuant(units="f"),
		harvest.spwn=FLQuant(),
		m.spwn	 =FLQuant()
	),
  validity=validFLStock
)	# }}}

setValidity("FLStock", validFLStock)
remove(validFLStock)


## Methods :: FLStock() 

FLStock <- function(name, desc, iniFLQuant, quant = "missing"){
	if (missing(name))
		name <- character(0)
	if (missing(desc))
		desc <- character(0)
	if (missing(iniFLQuant))
		iniFLQuant <- FLQuant()
	
	# Initialize FLQuant slots; dim, dimnames as iniFLQuant
	if (!inherits(iniFLQuant, "FLQuant"))
		stop("iniFLQuant must be an FLQuant object")
	# Template FLQuant for age-structured slots
		mat <- iniFLQuant
		mat[,,,,] <- NA
	# Template for quant-aggregated FLQuants
	names <- dimnames(mat)
	agr <- FLQuant(array(NA, dim=c(1,dim(mat)[2:5]), dimnames=list('all',
		year=names[[2]], unit=names[[3]], season=names[[4]], area=names[[5]])))
	quant(agr) <- quant(mat)
	Par <- dims(iniFLQuant)

	return(new("FLStock",
		name		= name,
		desc		= desc,
		catch	    = agr,
		catch.n	    = mat,
		catch.wt	= mat,
		discards	= agr,
		discards.n  = mat,
		discards.wt = mat,
		landings	= agr,
		landings.n  = mat,
		landings.wt = mat,
		stock	    = agr,
		stock.n	    = mat,
		stock.wt	= mat,
		mat		    = mat,
		m		    = mat,
		harvest	    = mat,
		harvest.spwn= mat,
		m.spwn	    = mat,
		range	    = unlist(list(min=Par$min, max=Par$max,
			plusgroup=NA, minyear=Par$minyear, maxyear=Par$maxyear)))
	)
}

# Test if an object is of FLStock class
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))

## landings
if (!isGeneric("landings")) {
	setGeneric("landings", function(object, ...){
		value <- standardGeneric("landings")
		value
	})
}

setMethod("landings", signature(object="FLStock"),

	function(object) {


#		if (overwrite || !any(complete.cases(slot(object, "landings")))) {
        if(!any(complete.cases(slot(object, "landings.n"))))
            warning("landings.n are needed to estimate total landings")
        if(!any(complete.cases(slot(object, "landings.wt"))))
      	    warning("landings.wt are needed to estimate total landings")

        if(all(dim(slot(object, "landings.n")) == dim(slot(object, "landings.n")))) {
            res <- apply(slot(object, "landings.n")*slot(object, "landings.wt"), 2:5, sum)
            units(res) <- units(slot(object, "landings.wt"))
    				quant(res) <- quant(slot(object, "landings.wt"))
            return(res)
        }
        else {
            warning("dimensions of landings.n and landings.wt are not the same")
			  }
  }
)


## discards
if (!isGeneric("discards")) {
	setGeneric("discards", function(object, ...){
		value <- standardGeneric("discards")
		value
	})
}

setMethod("discards", signature(object="FLStock"),

	function(object) {


#		if (overwrite || !any(complete.cases(slot(object, "discards")))) {
        if(!any(complete.cases(slot(object, "discards.n"))))
            warning("discards.n are needed to estimate total discards")
        if(!any(complete.cases(slot(object, "discards.wt"))))
      	    warning("discards.wt are needed to estimate total discards")

        if(all(dim(slot(object, "discards.n")) == dim(slot(object, "discards.n")))) {
            res <- apply(slot(object, "discards.n")*slot(object, "discards.wt"), 2:5, sum)
            units(res) <- units(slot(object, "discards.wt"))
				    quant(res) <- quant(slot(object, "discards.wt"))
            return(res)
        }
        else {
            warning("dimensions of discards.n and discards.wt are not the same")
			  }
  }
)

## catch
if (!isGeneric("catch")) {
	setGeneric("catch", function(object, ...){
		value <- standardGeneric("catch")
		value
	})
}

setMethod("catch", signature(object="FLStock"),

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
          
      res <- (slot(object, "landings.wt") * slot(object, "landings.n") +
              slot(object, "discards.wt") * slot(object, "discards.n")) /
              (slot(object, "landings.n") + slot(object, "discards.n"))
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


## summary
setMethod("summary", signature(object="FLStock"),
	function(object, ...){
		cat("An object of class \"FLStock\"\n\n")
		cat("Name:", object@name, "\n")
		cat("Description:", object@desc, "\n")
		cat("Range:\tmin\tmax\tp+group\tminyear\tmaxyear\n")
		cat("", object@range, "\n", sep="\t")
		cat("Quant:", quant(object@catch), "\n\n")
		
		for (s in list("catch", "catch.n", "catch.wt", "discards", "discards.n",
					   "discards.wt", "landings", "landings.n", "landings.wt",
					   "stock", "stock.n", "stock.wt", "harvest",
					   "harvest.spwn", "m", "m.spwn", "mat")) {
				 if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
		}
	}
)

## plot (make specific plots to display units graphically)
setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, ...){
		dots <- list(...)
		obj <- FLQuants(list(catch=x@catch, landings=x@landings, discards=x@discards))
		condnames <- names(dimnames(x@catch)[c(3:5)][dim(x@catch)[c(3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)
		formula <- formula(paste("data~year", cond))
		dots$x <- formula
		dots$data <- obj
		dots$ylab <- units(x@catch)
		dots$auto.key <- TRUE
		dots$type <- c("l")	
		do.call("xyplot", dots)
#		xyplot(x=formula, data=obj, auto.key=T, type="l", ...)
	}
)

# Test if an object is of FLStock class
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))


## window
if (!isGeneric("window")) {
	setGeneric("window", function(x, ...){
		value  <-  standardGeneric("window")
		value
	})
}
setMethod("window", signature(x="FLStock"),
	  function(x, start, end, extend=TRUE, frequency=1) {

names. <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		
		for (s. in names.) {
			slot(x, s.) <- window(slot(x, s.), start=start, end=end,
									   extend=extend, frequency=frequency)
			x@range["minyear"] <- start
			x@range["maxyear"] <- end
		}
		return(x)
	}
)


## setPlusGroup function - Robert Scott
#  changes the level of the plus group of the stock object
# Last Change dorleta : 11/04/2005

setPlusGroup <- function(stock, plusgroup="missing") {

	pg.wt.mean <-c("catch.wt","landings.wt","discards.wt")
	pg.truncate<-c("harvest","m","mat","harvest.spwn","m.spwn")
	pg.sum	 <-c("catch.n", "landings.n", "discards.n")

	#Check argument
	if (!inherits(stock, "FLStock"))
		stop("stock must be an 'FLStock' object!")

	# plusgroup calculations
	# xxxx.wt's etc. are condensed into the +gp using the catch if
	# stock.n not available
	# If stock.n available then these are used for f, m, mat & stockwt

	names. <- names(getSlots(class(stock))[getSlots(class(stock))=="FLQuant"])
	maxage <-  stock@range["max"]
	minage <-  stock@range["min"]

	#check plusgroup valid
	if (!missing(plusgroup)){
	  if(plusgroup > maxage){
		 return("Error : new plus group greater than oldest age")
	  }
	  else{
		if(plusgroup < minage)
		  return("Error : new plus group less than youngest age")
	  }
	}
	else
	  return(stock)

	#Are stock numbers present?
	stock.n.exist <- sum(stock@stock.n, na.rm=TRUE) > 1
	if(stock.n.exist) {
		pg.wt.mean <- c(pg.wt.mean, pg.truncate, "stock.wt")
		pg.wt.by   <- c(pg.sum, rep("stock.n", 7))
		pg.sum	 <- c(pg.sum, "stock.n")
	} else {
		pg.wt.mean <- c(pg.wt.mean, "stock.wt")
		pg.truncate<- c(pg.truncate, "stock.n")
		pg.wt.by   <- c(pg.sum, rep("catch.n",7))
	}

	#Perform +grp calcs
	#there are three options wt by stock.n, catch.n or simply add up the values

	calc.pg <- function(s., i., k., r., pg., action) {
		q.<-slot(s.,i.)

		minage <- s.@range["min"]

		#first check that object actually has ages
		a.<-dimnames(q.)[[quant(q.)]]

		if (any(a.=="all"))
		  return(q.)

		if (any(is.na(a.)) | !all(as.integer(a.)==sort(as.integer(a.))))
		  return(q.)

		pospg <- pg. - as.numeric(dimnames(slot(s., i.))[[1]][1]) + 1

		if (action == "sum"){
		  q.[pospg,,,,]<-apply(q.[r.,],2,sum)
		}
		else{
		  if(action == "wt.mean"){
			sum.r <- apply(slot(s.,k.)[r.,],2,sum)
			q.[pospg,,,,]<- ifelse( sum.r == 0, 0, apply(q.[r.,]*slot(s.,k.)[r.,],2,sum)/sum.r)
		  }
		}

		a. <- dimnames(q.)
		q. <- q.[1:pospg,,,,]
		dimnames(q.)[[1]] <- minage:pg.
		dimnames(q.)[2:5] <- a.[2:5]
		
		return(q.)
	}


  #  pg.range <- as.character(plusgroup:stock@range["max"])
	pg.range <- which((stock@range[1] : stock@range[2]) == plusgroup):length((stock@range[1] : stock@range[2]))
	stock@range["plusgroup"]<-plusgroup
	stock@range["max"]	  <-plusgroup

	#do the weighted stuff first
	for (i in 1 : length(pg.wt.mean)) {
		j <- pg.wt.mean[i]
		k <- pg.wt.by[i]
		slot(stock, j) <- calc.pg(stock, j, k, pg.range, plusgroup, "wt.mean")
	}

	#sum up stuff next
	for (i in pg.sum) {
	  slot(stock, i) <- calc.pg(stock, i, k, pg.range, plusgroup, "sum")
	}

	#then truncate stuff
	if (!stock.n.exist) {
	  for (i in pg.truncate) {
		 slot(stock, i) <- calc.pg(stock, i, k, pg.range, plusgroup, "truncate")
		}
	}

	return(stock)
}


## ssb
if (!isGeneric("ssb")) {
	setGeneric("ssb", function(object, ...){
		value <- standardGeneric("ssb")
		value
	})
}

setMethod("ssb", signature(object="FLStock"),
	function(object, ...) {

	if(units(slot(object, "harvest")) != 'f' && units(slot(object, "harvest")) != 'hr')
		stop("Correct units not specified in the harvest slot")
	if(units(slot(object, "harvest")) == 'f')
		return(apply(slot(object, "stock.n") * exp(-harvest(object, "f") *
        	slot(object,"harvest.spwn")-slot(object, "m")*
			slot(object, "m.spwn"))*slot(object, "stock.wt")*
			slot(object, "mat"), 2:5, sum))
	if(units(slot(object, "harvest")) == 'hr')
		return(apply(slot(object, "stock.n") * (1 - slot(object, "harvest") * slot(object, "harvest.spwn")) *
			exp(-slot(object, "m") * slot(object, "m.spwn")) * slot(object, "stock.wt") * slot(object, "mat"), 2:5, sum))	
	}
)


## sop
sop <- function(stock, slot) {
	return(apply(slot(stock, paste(slot, ".n", sep=""))*slot(stock, paste(slot, ".wt", sep="")), 2:5, sum, na.rm=TRUE)/ slot(stock, slot))
}


## transform(FLStock)
if (!isGeneric("transform")) {
	setGeneric("transform", function(x, ...){
		value <- standardGeneric("transform")
		value
	})
}

setMethod("transform", signature(x="FLStock"),
	function(x, ...) {

		args <- list(...)

		for (i in 1:length(args)) {
			slot(x, names(args)[i])[,,,,] <- args[[i]][,,,,]
		}

		return(x)
	}
)

## apply(FLStock)
setMethod("apply", signature(X="FLStock", MARGIN="list", FUN="function"),
	function(X, MARGIN, FUN, ...) {

		for(i in 1:length(MARGIN)) {
			slot(X, MARGIN[[i]]) <- apply(slot(X, MARGIN[[i]]), 1:5, FUN, ...)
		}
		return(X)
	}
)

## as.FLStock
if (!isGeneric("as.FLStock")) {
	setGeneric("as.FLStock", function(object, ...){
		value <- standardGeneric("as.FLStock")
		value
	})
}

## harvest
if (!isGeneric("harvest")) {
	setGeneric("harvest", function(object, ...){
		value <- standardGeneric("harvest")
		value
	})
}

setMethod("harvest", signature(object="FLStock"),
	function(object, index="f") {

		if (!missing(index) && units(slot(object, "harvest")) != index)
			stop("The units of harvest in the object do not match the specified index")

		return(slot(object, "harvest"))

	}
)

## harvest<-
if (!isGeneric("harvest<-")) {
	setGeneric("harvest<-", function(object, value){
		value <- standardGeneric("harvest<-")
		value
	})
}

setMethod("harvest<-", signature(object="FLStock", value="character"),
	function(object, value) {

	units(slot(object, "harvest")) <- value
	return(object)

	}
)
