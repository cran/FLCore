# FLStock.R - FLStock class and methods

# Author: FLR Team
# Maintainer: Rob Scott, CEFAS
# Additions:
# Last Change: 10 Apr 2007 20:22
# $Id: FLStock.R,v 1.42.2.31.2.5 2007/04/10 18:22:37 imosqueira Exp $

# Reference:
# Notes:

## class :: FLStock			{{{
validFLStock <- function(object) {

	## FLQuant objects that must have same dimensions
	dim <- dim(object@catch.n)

	s <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		"landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
		"m.spwn")
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
	
	s  <- list("catch", "landings", "discards", "stock")

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
		name	= character(0),
		desc	= character(0),
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=0, maxyear=0)),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(),
		mat		 = FLQuant(),
		harvest	 = FLQuant(units="f"),
		harvest.spwn = FLQuant(),
		m.spwn	 = FLQuant()
	)#,
#  validity=validFLStock
)
setValidity("FLStock", validFLStock)
remove(validFLStock)

invisible(createFLAccesors(new("FLStock"), exclude=c('range', 'harvest', 'name')))	# }}}

## FLStock() 	{{{
FLStock <- function(name=character(0), desc=character(0), plusgroup=NA, quant='quant', ...){

	args <- list(...)

	if(length(args)==0)
		args <- list(catch.n=FLQuant())

	# Set FLQuant dimensions
	dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])

	# template FLQuants
	if (missing(quant))
		quant <- names(dimnames)[1]
	iniFLQ <- FLQuant(dimnames=dimnames, quant=quant)
	agrFLQ <- FLQuant(dimnames=c(quant='all', dimnames(iniFLQ)[-1]), quant=quant)

	dims <- dims(iniFLQ)

	res <- new("FLStock",
		name		= name,
		desc		= desc,
		catch	    = agrFLQ,
		catch.n	    = iniFLQ,
		catch.wt	= iniFLQ,
		discards	= agrFLQ,
		discards.n  = iniFLQ,
		discards.wt = iniFLQ,
		landings	= agrFLQ,
		landings.n  = iniFLQ,
		landings.wt = iniFLQ,
		stock	    = agrFLQ,
		stock.n	    = iniFLQ,
		stock.wt	= iniFLQ,
		mat		    = iniFLQ,
		m		    = iniFLQ,
		harvest	    = iniFLQ,
		harvest.spwn= iniFLQ,
		m.spwn	    = iniFLQ,
		range       = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear)))
	
	# Load given slots
	for(i in names(args))
		if (i != 'iniFLQuant')
			slot(res, i) <- args[[i]]

	return(res)
}	# }}}

# is.FLStock	{{{
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))	# }}}

## computeLandings	{{{
if (!isGeneric("computeLandings")) {
	setGeneric("computeLandings", function(object, ...){
		value <- standardGeneric("computeLandings")
		value
	})
}
setMethod("computeLandings", signature(object="FLStock"),
	function(object, na.rm=TRUE) {
        res <- quantSums(landings.n(object)*landings.wt(object), na.rm=na.rm)
        units(res) <- paste(units(landings.n(object)), units(landings.wt(object)))
        return(res)
 	} 
)	# }}}

## computeDiscards	{{{
if (!isGeneric("computeDiscards")) {
	setGeneric("computeDiscards", function(object, ...){
		value <- standardGeneric("computeDiscards")
		value
	})
}
setMethod("computeDiscards", signature(object="FLStock"),
	function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        units(res) <- paste(units(discards.n(object)), units(discards.wt(object)))
        return(res)
 	} 
)	# }}}

## computeCatch	{{{
if (!isGeneric("computeCatch")) {
	setGeneric("computeCatch", function(object, ...){
		value <- standardGeneric("computeCatch")
		value
	})
}
setMethod("computeCatch", signature(object="FLStock"),
    function(object, slot="catch", na.rm=TRUE) {
        
        if(slot == "n"){
		# NA to 0
        	res <- landings.n(object) + discards.n(object)
            if (units(discards.n(object)) == units(landings.n(object)))
				units(res) <- units(discards.n(object))
        }
        else if(slot == "wt"){
        	res <- (landings.wt(object) * landings.n(object) +
        		discards.wt(object) * discards.n(object)) /
        	 	(landings.n(object) + discards.n(object))
			if (units(discards.wt(object)) == units(landings.wt(object)))
				units(res) <- units(discards.wt(object))
        }
		else if (slot == "all"){
			res <- FLQuants(catch=computeCatch(object, slot="catch"),
				catch.wt=computeCatch(object, slot="wt"),
				catch.n=computeCatch(object, slot="n"))
		}
        else {
			res <- quantSums(catch.n(object) * catch.wt(object), na.rm=na.rm)
                        units(res) <- paste(units(catch.n(object)), units(catch.wt(object)))
        }
		return(res)
    }
)	# }}}

## summary		{{{
setMethod("summary", signature(object="FLStock"),
	function(object, ...){
		cat("An object of class \"FLStock\"\n\n")
		cat("Name:", object@name, "\n")
		cat("Description:", object@desc, "\n")
		cat("Range:\tmin\tmax\tp+group\tminyear\tmaxyear\n")
		cat("", object@range, "\n", sep="\t")
		cat("Quant:", quant(object@catch), "\n\n")
		
		for (s in names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])) {
			if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
		}
	}
)	# }}}

## plot (make specific plots to display units graphically)  {{{
setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, ...){
		dots <- list(...)
		obj <- FLQuants(list(catch=catch(x), landings=landings(x), discards=discards(x)))
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
	}
)	# }}}

## is.FLStock	{{{
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))	# }}}

## window	{{{
if (!isGeneric("window")) {
	setGeneric("window", useAsDefault = window)
}
setMethod("window", signature(x="FLStock"),
	  function(x, start, end, extend=TRUE, frequency=1) {

		names. <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		
		for (s. in names.) {
			slot(x, s.) <- window(slot(x, s.), start=start, end=end,
									   extend=extend, frequency=frequency)
    }
		x@range["minyear"] <- start
		x@range["maxyear"] <- end

		return(x)
	}
)	# }}}

## setPlusGroup function	{{{
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
}	# }}}

## ssb		{{{
if (!isGeneric("ssb")) {
	setGeneric("ssb", function(object, ...){
		value <- standardGeneric("ssb")
		value
	})
}

setMethod("ssb", signature(object="FLStock"),
	function(object, ...) {

	if(units(harvest(object)) == 'f')
		# 
		return(quantSums(stock.n(object) * exp(-harvest(object) *
        	harvest.spwn(object) - m(object) * m.spwn(object)) *
			stock.wt(object) * mat(object)))
	else if(units(harvest(object)) == 'hr')
		return(quantSums(stock.n(object) * (1 - harvest(object) * harvest.spwn(object)) *
			exp(-m(object) * m.spwn(object)) * stock.wt(object) * mat(object)))	
	else
		stop("Correct units (f or hr) not specified in the harvest slot")
	}
)	# }}}

## sop	{{{
sop <- function(stock, slot="catch") {
	return(quantSums(slot(stock, paste(slot, ".n", sep="")) *
		slot(stock, paste(slot, ".wt", sep=""))) / slot(stock, slot))
}	# }}}

## transform(FLStock)	{{{
if (!isGeneric("transform")) {
	setGeneric("transform", function(`_data`, ...){
		value <- standardGeneric("transform")
		value
	})
}
setMethod("transform", signature(`_data`="FLStock"),
	function(`_data`, ...) {
		args <- list(...)
		for (i in 1:length(args)) {
			slot(`_data`, names(args)[i]) <- args[[i]]
		}
		if(validObject(`_data`))
    		return(`_data`)
        else
            stop('Modified slots invalidate object')
	}
)	# }}}

## apply(FLStock)	{{{
setMethod("apply", signature(X="FLStock", MARGIN="vector", FUN="function"),
    function(X, MARGIN, FUN, ...) {

    names.   <- names(getSlots(class(X))[getSlots(class(X))=="FLQuant"])
    names.t  <- c("catch",   "landings",   "discards",   "stock")
    names.n  <- c("catch.n", "landings.n", "discards.n", "stock.n")
    names.wt <- c("catch.wt","landings.wt","discards.wt","stock.wt")
    
    names.avg <- names.[!is.element(names., c(names.t,names.n,names.wt))]

    # weighted averages by stock.n or catch.n
    wt <- "stock.n"
    if(all(is.na(slot(X,"stock.n"))))
      wt <- "catch.n"
    
    for(s. in names.avg)
      slot(X, s.) <- apply(slot(X,s.)*slot(X,wt),MARGIN,FUN, ...)/apply(slot(X,wt),MARGIN,FUN,...)
    
    # weighted averages by catch.n, landings.n, discards.n
    for(i in 1:(length(names.wt)-1))
      slot(X, names.wt[i]) <- apply(slot(X,names.wt[i])*slot(X,names.n[i]),MARGIN,FUN, ...)/apply(slot(X,names.n[i]),MARGIN,FUN,...)

    # treat stock.wt separately depending on NA or not in stock.n
    slot(X, "stock.wt") <- apply(slot(X,"stock.wt")*slot(X,wt),MARGIN,FUN, ...)/apply(slot(X,wt),MARGIN,FUN,...)
    
    for(s. in c(names.t, names.n))
			slot(X, s.) <- apply(slot(X, s.), MARGIN, FUN, ...)

    # make changes to range object
    if(!is.element(1,MARGIN)) {
      X@range["min"] <- 1
      X@range["max"] <- 1
      X@range["plusgroup"] <- NA
    }
    if(!is.element(2,MARGIN)) {
      X@range["minyear"] <- 1
      X@range["maxyear"] <- 1
    }
		return(X)
	}
)	# }}}

## as.FLStock	{{{
if (!isGeneric("as.FLStock")) {
	setGeneric("as.FLStock", function(object, ...){
		value <- standardGeneric("as.FLStock")
		value
	})
}	# }}}

## harvest		{{{
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
setMethod("harvest<-", signature(object="FLStock", value="FLQuant"),
	function(object, value) {
		slot(object, "harvest") <- value
		return(object)
	}
)	# }}}

## catch<- FLQuants		{{{
setMethod("catch<-", signature(object="FLStock", value="FLQuants"),
	function(object, value) {
		catch(object) <- value[['catch']]
		catch.n(object) <- value[['catch.n']]
		catch.wt(object) <- value[['catch.wt']]
		return(object)
	}
) # }}}

## trim     {{{
setMethod("trim", signature("FLStock"), function(object, ...){

	args <- list(...)

    c1 <- args[[quant(object@stock.n)]]
	c2 <- args[["year"]]
	c3 <- args[["unit"]]
	c4 <- args[["season"]]
	c5 <- args[["area"]]
	c6 <- args[["iter"]]

    # FLQuants with quant
	names <- names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])

    for (name in names) {
        if(name %in% c('stock', 'catch', 'landings', 'discards'))
            slot(object,name) <- trim(slot(object,name), year=c2, unit=c3, season=c4,
                area=c5, iter=c6)
        else
            slot(object,name) <- trim(slot(object,name), age=c1, year=c2, unit=c3,
                season=c4, area=c5, iter=c6)
    }
            
  if (length(c1) > 0) {
    object@range["min"] <- c1[1]
    object@range["max"] <- c1[length(c1)]
    object@range["plusgroup"] <- NA
  }
  if (length(c2)>0 ) {
    object@range["minyear"] <- c2[1]
    object@range["maxyear"] <- c2[length(c2)]
  }

	return(object)

}) # }}}

## units(FLStock)	{{{
setMethod("units", signature(x="FLStock"), function(x) {
	
	names. <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
	res <- list()
	for(s. in names.)
    res <- as.list(c(unlist(res), assign(s., units(slot(x, s.)))))
  names(res) <- names.
  return(res)
})  #}}}

## units<-(FLStock)  {{{
setMethod("units<-", signature(x="FLStock", value="list"),
    function(x, value) {
        for(i in seq(along=value))
            if(is.character(value[[i]]))
                units(slot(x, names(value[i]))) <- value[[i]]
        return(x)
	}
) # }}}

## SSB per unit recruit {{{
if (!isGeneric("ssbpurec")) {
	setGeneric("ssbpurec", function(object, ...){
		value <- standardGeneric("ssbpurec")
		value
	})
}

setMethod("ssbpurec",signature(object="FLStock"),
	function(object, start = "missing", end = "missing", type = "non-param", recs = "missing", spwns = "missing", plusgroup = TRUE, ...) {

		# checks and chose the range over which we calculate the SSB per unit recruit
		
		if((missing(start) && !missing(end)) | (!missing(start) && missing(end))) 
			stop("Error in ssbpurec: start and end must be supplied together if at all")

		if(missing(start) && missing(end)) 
			x  <- window(object,dims(object@m)[['minyear']],dims(object@m)[['minyear']])

		if(!missing(start) && !missing(end))
			x  <- window(object,start,end)

		if(missing(recs)) 
			recs  <- 1
		if(missing(spwns)) 
			spwns  <- 1 				

		ymin  <- dims(x@m)[['minyear']]
		ymax  <- dims(x@m)[['maxyear']]
		ns  <- dims(x@m)[['season']]
		amin  <- dims(x@m)[['min']]
		amax  <- dims(x@m)[['max']]
		pg  <- dim(x@m)[1]
		
		# if amin = 0 and recs < spwns !!!! cannot happen

		if(amin == 0 && recs < spwns)
			stop("Error: minimum age is zero and the recruitment occurs before spawning - not possible")
		
		if(type == 'non-param') {
			
			m  <- yearMeans(slot(x, "m"))
			mat  <- yearMeans(slot(x, "mat")) 
			wt  <- yearMeans(slot(x, "stock.wt"))
			n  <- FLQuant(m)
			
			# seasonal or non-seasonal options
			
			if(ns == 1) {
				
				# standard calculation : recruitment = 1, natural mort. sets the 
				# age-structure, with or without a plusgroup

				n[1,1,,1,]  <- 1
				for(a in 2:pg) 
					n[a,1,,1,]  <- n[a-1,1,,1,] * exp(-m[a-1,1,,1,])
				if(plusgroup)
					n[pg,1,,1,]  <- n[pg,1,,1,] / (1-exp(-m[pg,1,,1,]))
				
				rho  <- quantSums(n * mat * wt)

				# always set dimnames$year to be the minyear in the stock object

				dimnames(rho)$year  <- dims(object@m)[['minyear']]
				
			} else {

				# to come...
			}
			
		}

		return(rho)
	}
)# }}}

# '['       {{{
setMethod('[', signature(x='FLStock'),
	function(x, i, j, k, l, m, ..., drop=FALSE) {

		if (missing(i))
			i  <-  seq(1, length(dimnames(x@stock.n@.Data)[1][[1]]))
		if (missing(j))
			j  <-  dimnames(x@stock.n@.Data)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@stock.n@.Data)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@stock.n@.Data)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@stock.n@.Data)[5][[1]]

	    quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		    "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
    		"m.spwn")
        for(q in quants)
            slot(x, q) <- slot(x, q)[i,j,k,l,m, drop=FALSE]

	    quants <- list("catch", "landings", "discards", "stock")
        for(q in quants)
            slot(x, q) <- slot(x, q)[1,j,k,l,m, drop=FALSE]
        
        # range
        x@range['min'] <- dims(slot(x, 'stock.n'))$min
        x@range['max'] <- dims(slot(x, 'stock.n'))$max
        x@range['minyear'] <- dims(slot(x, 'stock.n'))$minyear
        x@range['maxyear'] <- dims(slot(x, 'stock.n'))$maxyear

        return(x)
    }
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLStock"),
	function(x, i, j, k, l, m, ..., value="missing") {

        if(!is.FLStock(value))
            stop('Value must be of class FLStock')

		if (missing(i))
			i  <-  dimnames(x@stock.n)[1][[1]]
		if (missing(j))
			j  <-  dimnames(x@stock.n)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@stock.n)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@stock.n)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@stock.n)[5][[1]]

	    quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		    "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
    		"m.spwn")
        for(q in quants)
            slot(x, q)[i,j,k,l,m] <- slot(value, q)
	    
        quants <- list("catch", "landings", "discards", "stock")
        for(q in quants) {
            browser()
            slot(x, q)[1,j,k,l,m] <- slot(value,q)
            }

   		return(x)
	}
)   # }}}

# qapply    {{{
if (!isGeneric("qapply")) {
	setGeneric("qapply", function(X, FUN, ...){
		standardGeneric("qapply")
	})
}
setMethod('qapply', signature(X='FLStock', FUN='function'),
	function(X, FUN, ...) {
		FUN <- match.fun(FUN)
		slots <- names(getSlots(class(X))[getSlots(class(X))=='FLQuant'])
		if(is.FLQuant(do.call(FUN, list(slot(X,slots[1]), ...)))) {
			res <- X
			for (i in slots)
				slot(res, i) <- do.call(FUN, list(slot(X,i), ...))
		}
		else {
			res  <- vector('list', 0)
			for (i in slots)
				res[[i]] <- do.call(FUN, list(slot(X,i), ...))
		}
		return(res)
	}
)   # }}}

# as.data.frame		{{{
setMethod("as.data.frame", signature(x="FLStock"),
 	function(x, row.names="missing", optional="missing") {

 		slots <- names(getSlots(class(x))[getSlots(class(x))=='FLQuant'])
 		df <- cbind(as.data.frame(slot(x, slots[2])), slot=slots[2])
 		for (i in slots[-2])
 			df <- rbind(df, cbind(as.data.frame(slot(x, i)), slot=i))
 		# store metadata as attributes
 		attributes(df)$objectname <- x@name
 		attributes(df)$desc <- x@desc
 		attributes(df)$range <- x@range
 		attributes(df)$units <- unlist(units(x))

 		return(df)
 	}
)	# }}}
