# FLQuant.R - FLQuant class and methods

# Author: FLR Team
# Maintainer: Iago Mosqueira, AZTI Tecnalia
# Additions:
# Last Change: 30 ene 2007 10:29
# $Id: FLQuant.R,v 1.52.2.53.2.3 2007/01/30 12:15:56 imosqueira Exp $

# Reference:
# Notes:

## Class
## FLQuant     {{{
validFLQuant  <-  function(object){

	# Make sure there are at least 5 dimensions in the array named
	# *, "year", "unit", "season" and "area"
	DimNames  <-  names(dimnames(object))
	if (length(DimNames) < 5)
		return("the array must have at least 5 dimensions")
	if (!all(DimNames[2:5] == c("year", "unit", "season", "area")))
		return("dimension names of the array are incorrect")
	if (!is.numeric(object) && !is.na(object))
		return("array is not numeric")

	# check "units" slot
	if(!is.character(object@units))
		return("units must be a string")
	
	# check contents are not integers
#    if(any(is.integer(object)))
#        return("FLQuants must be of type double or NA")

	# Everything is fine
	return(TRUE)
}

setClass("FLQuant",
	representation("array", units="character"),
	prototype(array(NA, dim=c(1,1,1,1,1),
        dimnames=list(quant="all", year="1", unit="unique", season="all", area="unique")),
        units="NA"),
	validity=validFLQuant
)

setValidity("FLQuant", validFLQuant)
remove(validFLQuant)        # }}}

## Methods
## FLQuant      {{{
if (!isGeneric("FLQuant")) {
	setGeneric("FLQuant", function(object, ...){
		value  <-  standardGeneric("FLQuant")
		value
	})
}   # }}}

# FLQuant(missing)		{{{
# FLQuant  <- FLQuant()
setMethod("FLQuant", signature(object="missing"),
	function(object, dim=rep(1,5), dimnames="missing", quant=NULL, units="NA") {
		
		# no dim or dimnames
		if (missing(dim) && missing(dimnames)) {
			dim <- c(1,1,1,1,1)
			dimnames <- list(quant='all', year=1, unit='unique', season='all', area='unique')
		}

		# dim missing
		else if (missing(dim)) {
			dimnames <- filldimnames(dimnames)
			dim <- as.numeric(sapply(dimnames, length))
#			quant <- names(dimnames)[1]
		}

		# dimnames missing
		else if (missing(dimnames)) {
			dim <- c(dim, rep(1,5))[1:5]
			dimnames <- list(
				quant=if(dim[1]==1){"all"}else{1:dim[1]},
				year=1:dim[2],
				unit=if(dim[3]==1){"unique"}else{1:dim[3]},
				season=if(dim[4]==1){"all"}else{1:dim[4]},
				area=if(dim[5]==1){"unique"}else{1:dim[5]})
		}
		# both
		else {
			dim <- c(dim, rep(1,5))[1:5]
			dimnames <- filldimnames(dimnames, dim=dim)
		}
		flq <- new("FLQuant", array(NA, dim=dim, dimnames=dimnames), units=units)

		if (!is.null(quant))
			quant(flq) <- quant

		return(flq)
	}
)	# }}}

# FLQuant(vector)		{{{
# FLQuant  <- FLQuant(vector)
setMethod("FLQuant", signature(object="vector"),
	function(object, dim=rep(1,5), dimnames="missing", quant=NULL, units="NA") {
		
		# no dim or dimnames
		if (missing(dim) && missing(dimnames)) {
			dim <- c(1,length(object),1,1,1)
			dimnames <- list(quant='all', year=1:length(object), unit='unique',
                season='all', area='unique')
		}

		# dim missing
		else if (missing(dim)) {
			dimnames <- filldimnames(dimnames)
			dim <- as.numeric(sapply(dimnames, length))
		}

		# dimnames missing
		else if (missing(dimnames)) {
			dim <- c(dim, rep(1,5))[1:5]
			dimnames <- list(
				quant=if(dim[1]==1){"all"}else{1:dim[1]},
				year=1:dim[2],
				unit=if(dim[3]==1){"unique"}else{1:dim[3]},
				season=if(dim[4]==1){"all"}else{1:dim[4]},
				area=if(dim[5]==1){"unique"}else{1:dim[5]})
		}
		# both
		else {
			dim <- c(dim, rep(1,5))[1:5]
			dimnames <- filldimnames(dimnames, dim=dim)
		}
		flq <- new("FLQuant", array(object, dim=dim, dimnames=dimnames), units=units)

		if (!is.null(quant))
			quant(flq) <- quant

		return(flq)
	}
)	# }}}

# FLQuant(array)		{{{
# FLQuant <- FLQuant(array)
setMethod("FLQuant", signature(object="array"),
	function(object, dim=rep(1,5), dimnames="missing", quant=NULL, units="NA") {
		# no dim or dimnames
		if (missing(dim) && missing(dimnames)) {
			# get dim from object and complete
			dim <- c(dim(object), rep(1,5))[1:5]
			# if object has dimnames, use then
			if(!is.null(dimnames(object))) {
				dimnames <- filldimnames(dimnames(object), dim=dim)
			}
			# otherwise create from dim
			else {
				dimnames <- list(quant=1:dim[1], year=1:dim[2], unit=1:dim[3],
					season=1:dim[4], area=1:dim[5])
				dimnames[which(dim==1)] <- list(quant='all', year=1, unit='unique', season='all',
					area='unique')[which(dim==1)]
			}
		}

		# dim missing
		else if (missing(dim)) {
			dimnames <- filldimnames(dimnames, dim=c(dim(object), rep(1,5))[1:5])
			# extract dim from dimnames
			dim <- c(dim(object),
                as.numeric(sapply(dimnames, length))[length(dim(object))+1:5])[1:5]
		}

		# dimnames missing
		else if (missing(dimnames)) {
			dim <- c(dim, rep(1,5))[1:5]
			# create dimnames from dim
			dimnames <- list(quant=1:dim[1], year=1:dim[2], unit=1:dim[3],
				season=1:dim[4], area=1:dim[5])
			dimnames[which(dim==1)] <- list(quant='all', year=1, unit='unique', season='all',
				area='unique')[which(dim==1)]
		}
		flq <- new("FLQuant", array(object, dim=dim, dimnames=dimnames), units=units)

		if (!is.null(quant))
			quant(flq) <- quant

		return(flq)
	}
)	# }}}

# FLQuant(matrix)		{{{
# FLQuant <- FLQuant(matrix)
setMethod("FLQuant", signature(object="matrix"),
	function(object, dim="missing", dimnames="missing", ...) {

		if(missing(dim))
			dim <- c(nrow(object), ncol(object), rep(1,5))[1:5]
		if(!missing(dimnames))
			return(FLQuant(array(object, dim=dim, dimnames=filldimnames(dimnames, dim=dim)), ...))
		if(!is.null(dimnames(object)) && missing(dimnames))
			return(FLQuant(array(object, dim=dim), dimnames=filldimnames(dimnames(object),
				dim=dim), ...))
		return(FLQuant(array(object, dim=dim), ...))
	}
)	# }}}

# FLQuant(FLQuant)		{{{
# FLQuant <- FLQuant(FLQuant)
setMethod("FLQuant", signature(object="FLQuant"),
	function(object, quant="quant", units="NA", dimnames=list()) {

        # generate dimnames
        dnames <- dimnames(object)
        dnames[names(dimnames)] <- dimnames
        
        # create empty FLQuant
        res <- FLQuant(dimnames=dnames)
		quant(res) <- if(!missing(quant)){quant} else {quant(object)}
		units(res) <- if(!missing(units)){units} else {units(object)}
        
        # assign existing data
        if(all(dnames[[1]]%in%dimnames(object)[[1]]))
            qnames <- dimnames(object)[[1]]
        else
            qnames <- 1:length(dimnames(object)[[1]])
        res[qnames,dimnames(object)[[2]],1:length(dimnames(object)[[3]]),1:length(dimnames(object)[[4]]),1:length(dimnames(object)[[5]])] <- object
        # listo!
		return(res)
	}
)		# }}}

## as.FLQuant      {{{
if (!isGeneric("as.FLQuant")) {
	setGeneric("as.FLQuant", function(x, ...){
		value  <-  standardGeneric("as.FLQuant")
		value
	})
}   # }}}

# as.FLQuant(array)		{{{
setMethod("as.FLQuant", signature(x="array"),
	function(x, ...) {
		return(FLQuant(x, ...))
	}
)		# }}}

# as.FLQuant(matrix)		{{{
setMethod("as.FLQuant", signature(x="matrix"),
	function(x, ...) {
		return(FLQuant(x, ...))
	}
)		# }}}

# as.FLQuant(FLQuant)		{{{
setMethod("as.FLQuant", signature(x="FLQuant"),
	function(x, ...) {
		return(FLQuant(x, ...))
	}
)		# }}}

# as.FLQuant(vector)		{{{
setMethod("as.FLQuant", signature(x="vector"),
	function(x, ...) {
		return(FLQuant(x, ...))
	}
)		# }}}

# as.FLQuant(data.frame)		{{{
setMethod("as.FLQuant", signature(x="data.frame"),
	function(x, dimnames="missing", quant="quant", units='NA') {

        # get data.frame names and compare
		names(x) <- tolower(names(x))
    	validnames <-c("year","unit","season","area","data")

		indices <- match(validnames, names(x))
	   	indices <- indices[!is.na(indices)]

        # get quant
    	qname <- names(x)
		qname[indices] <- NA
		qname <- qname[!is.na(qname)]

    	if (length(qname) > 1)
			stop("too many columns in data.frame")
        if(length(qname) == 0)
            qname <- "quant"
        
        # check and fill up missing dimensions
        n <- dim(x)[1]
        em <- data.frame(year=rep(1,n), unit=rep('unique', n),
            season=rep('all',n), area=rep('unique', n))
        em[names(x)] <- x
        names(em)[names(em)=="quant"] <- qname

        # create array
        flq <- tapply(em[,"data"], list(em[,qname], em[,"year"], em[,"unit"], em[,"season"],
            em[,"area"]), sum)

        # fix dimnames names
        names(dimnames(flq)) <- c(qname, 'year', 'unit', 'season', 'area')
    
        # create FLQuant
        flq <- FLQuant(flq)
        
        # fill up missing years
        if(length(dimnames(flq)[['year']]) != length(as.character(seq(dims(flq)$minyear,
            dims(flq)$maxyear)))) {
            res <- FLQuant(dimnames=c(dimnames(flq)[1], list(year=seq(dims(flq)$minyear,
                dims(flq)$maxyear)), dimnames(flq)[3:5]))
            res[,dimnames(flq)[['year']],] <- flq
            flq <- res
        }

        # add extra arguments
        if(!missing(dimnames))
			dimnames(flq) <- dimnames
		if(!missing(quant))
			quant(flq) <- quant
		if(!missing(units))
			units(flq) <- units

		return(flq)
    }
)		# }}}

## filldimnames       {{{
filldimnames <- function(dnames, dim=rep(1,5)) {
	# check only one name for quant in input
	if(length(names(dnames)[!names(dnames)%in%c("year","unit","season","area")]) > 1)
		stop("more than one vector of names given for the first dimension")
	# generate standard names for given dimensions
	xnames <- dimnames(FLQuant(dim=dim))
	for(i in 1:length(dnames)) {
		# non-quant names
		if(any(names(dnames)[i]==c("year","unit","season","area")))
			xnames[[names(dnames)[i]]] <- dnames[[i]]
		# quant
		else {
			xnames[[1]] <- dnames[[i]]
			names(xnames)[1] <- names(dnames)[i]
		}
	}
	return(xnames)
} # }}}

## quant        {{{
if (!isGeneric("quant")) {
	setGeneric("quant", function(object, ...){
		value  <-  standardGeneric("quant")
		value
	})
}

setMethod("quant", signature(object="FLQuant"),
	function(object) {
		return(names(dimnames(object))[1])
	}
) # }}}

## quant<-      {{{
if (!isGeneric("quant<-")) {
	setGeneric("quant<-", function(object, value){
		value  <-  standardGeneric("quant<-")
		value
	})
}

setMethod("quant<-", signature(object="FLQuant"),
	function(object, value) {
		dn <- dimnames(object)
		names(dn)[1] <- value
		attributes(object)$dimnames <- dn
		return(object)
	}
) # }}}

## units        {{{
if (!isGeneric("units")) {
	setGeneric("units", function(object, ...){
		value  <-  standardGeneric("units")
		value
	})
}

setMethod("units", signature(object="FLQuant"),
	function(object)
		return(object@units)
) # }}}

## units<-      {{{
if (!isGeneric("units<-")) {
	setGeneric("units<-", function(object, value){
		value  <-  standardGeneric("units<-")
		value
	})
}

setMethod("units<-", signature(object="FLQuant", value="character"),
	function(object, value) {
		if (!inherits(object, "FLQuant"))
			return(object)
		if (!is(value, "character"))
			stop("'units' must be a character string")
		object@units <- value
		return(object)
	}
) # }}}

## names         {{{
if (!isGeneric("names")) {
	setGeneric("names", useAsDefault = names)
}
setMethod("names", signature(x="FLQuant"),
	function(x) names(dimnames(x)))
# }}}

## dimnames<-       {{{
setMethod("dimnames<-", signature(x="FLQuant"),
	function(x, value) {
		if(length(names(value)[!names(value)%in%c("year","unit","season","area")]) > 1)
			stop("more than one vector of names given for the first dimension")
		xnames <- dimnames(x)
		for(i in 1:length(value)) {
			if(any(names(value)[i]==c("year","unit","season","area")))
				xnames[[names(value)[i]]] <- value[[i]]
			else {
				xnames[[1]] <- value[[i]]
				names(xnames)[1] <- names(value)[i]
			}
		}
		attributes(x)$dimnames <- xnames
		return(x)
	}
) # }}}

## dims       {{{
if (!isGeneric("dims")) {
	setGeneric("dims", function(obj, ...){
		value  <-  standardGeneric("dims")
		value
	})
}

setMethod("dims", signature(obj="FLQuant"),
	# Return a list with different parameters
	function(obj, ...){
		quant   <-  as.numeric(dim(obj)[names(obj) == quant(obj)])
		min	 <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][1]))
		max	 <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][length(dimnames(obj)[[quant(obj)]])]))
		year	<-  as.numeric(dim(obj)[names(obj) == "year"])
		minyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[1]))
		maxyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[dim(obj)[names(obj) == "year"]]))
		unit	<-  dim(obj)[names(obj) == "unit"]
 		season  <-  dim(obj)[names(obj) == "season"]
		area	<-  dim(obj)[names(obj) == "area"]
		list <- list(quant=quant, min=min, max=max, year=year, minyear=minyear,
			maxyear=maxyear, unit=unit, season=season, area=area)
		names(list)[1] <- quant(obj)
		return(list)
	}
)   # }}}

## is.FLQuant       {{{
is.FLQuant  <-  function(x)
	return(is(x, "FLQuant"))
# }}}

## show 	{{{
setMethod("show", signature(object="FLQuant"),
	function(object){
		cat("An object of class \"FLQuant\":\n\n")
		print(unclass(object))
	}
)   # }}}

## print 	{{{
setMethod("print", signature(x="FLQuant"),
	function(x){
		cat("An object of class \"FLQuant\":\n\n")
		print(unclass(x))
	}
)   # }}}

## summary          {{{
if (!isGeneric("summary")) {
	setGeneric("summary", useAsDefault = summary)
}
setMethod("summary", signature(object="FLQuant"),
	function(object, ...){
		cat("An object of class \"FLQuant\" with:\n")
		cat("dim  : ", dim(object), "\n")
		cat("quant: ", quant(object), "\n")
		cat("units: ", units(object), "\n\n")
		cat("Min    : ", min(object), "\n")
		cat("1st Qu.: ", quantile(as.vector(object), 0.25), "\n")
		cat("Mean   : ", mean(as.vector(object), na.rm=TRUE), "\n")
		cat("Median : ", median(as.vector(object), na.rm=TRUE), "\n")
		cat("3rd Qu.: ", quantile(as.vector(object), 0.75), "\n")
		cat("Max    : ", max(object), "\n")
	}
)   # }}}

## plot     {{{
if (!isGeneric("plot")) {
	setGeneric("plot", useAsDefault = plot)
}

setMethod("plot", signature(x="FLQuant", y="missing"),
	function(x, xlab="year", ylab=paste("data (", units(x), ")", sep=""),
		type='bar', ...) {

		if (type == 'l' | type == 'b' | type == 'p') {
			# get dimensions to condition on (skip quant)
			condnames <- names(dimnames(x)[c(3:5)][dim(x)[c(3:5)]!=1])
			cond <- paste(condnames, collapse="+")
			if(cond != "") cond <- paste("|", cond)
			formula <- formula(paste("data~year", cond))
			# set strip to show conditioning dimensions names
			strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

			xyplot(formula, data=x, groups=subset(as.data.frame(x), select=quant(x))[,1],
				xlab=xlab, ylab=ylab, strip=strip,
				key = simpleKey(levels(subset(as.data.frame(x), select=quant(x))[,1]),
				space = "right"), type=type, ...)
		}
		else {

			# get dimensions to condition on (length !=1)
			condnames <- names(dimnames(x)[c(1,3:5)][dim(x)[c(1,3:5)]!=1])
			cond <- paste(condnames, collapse="+")
			if(cond != "") cond <- paste("|", cond)
			formula <- formula(paste("data~year", cond))
			# set strip to show conditioning dimensions names
			strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

	# using do.call to avoid eval of some arguments
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- x
	lst$x <- formula
	lst$xlab <- xlab
	lst$ylab <- ylab
	lst$strip <- strip
	
	do.call("xyplot", lst)

		}
	}
)   # }}}

## lattice plots	{{{
# xyplot

if (!isGeneric("xyplot")) {
	setGeneric("xyplot", useAsDefault = xyplot)
}

setMethod("xyplot", signature("formula", "FLQuant"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
})

# bwplot
if (!isGeneric("bwplot")) {
	setGeneric("bwplot", useAsDefault = bwplot)
}


setMethod("bwplot", signature("formula", "FLQuant"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("bwplot", lst)
})

# dotplot
if (!isGeneric("dotplot")) {
	setGeneric("dotplot", useAsDefault = dotplot)
}


setMethod("dotplot", signature("formula", "FLQuant"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("dotplot", lst)

})

# barchart
if (!isGeneric("barchart")) {
	setGeneric("barchart", useAsDefault = barchart)
}


setMethod("barchart", signature("formula", "FLQuant"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("barchart", lst)

})

# stripplot
if (!isGeneric("stripplot")) {
	setGeneric("stripplot", useAsDefault = stripplot)
}


setMethod("stripplot", signature("formula", "FLQuant"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("stripplot", lst)

})

# histogram
if (!isGeneric("histogram")) {
	setGeneric("histogram", useAsDefault = histogram)
}


setMethod("histogram", signature("formula", "FLQuant"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("histogram", lst)

})

# bubbles
setGeneric("bubbles", function(x, data, ...){
    standardGeneric("bubbles")
    }
)

setMethod("bubbles", signature(x="formula", data ="FLQuant"),
function(x, data, bub.scale=2.5, ...){
	dots <- list(...)
	data <- as.data.frame(data)
	dots$data <- data
	dots$cex <- bub.scale*(data$data/max(data$data, na.rm=T))+bub.scale*0.1
	dots$panel <- function(x, y, ..., cex, subscripts){
		panel.xyplot(x, y, cex=cex[subscripts], ...)
	}
	call.list <- c(x=x, dots)
	ans <- do.call("xyplot", call.list)
	ans
})

# }}}

## "["             {{{
setMethod("[", signature(x="FLQuant"),
	function(x, i, j, k, l, m, ..., drop=FALSE) {

		if (missing(i))
			i  <-  seq(1, length(dimnames(x@.Data)[1][[1]]))
		if (missing(j))
			j  <-  dimnames(x@.Data)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@.Data)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@.Data)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@.Data)[5][[1]]

   		if (drop==FALSE) {
	  		flq	 <- FLQuant(x@.Data[i, j, k, l, m, drop=FALSE])
			units(flq) <- units(x)
			quant(flq) <- quant(x)
		}
		else if(drop==TRUE)
			flq  <- x@.Data[i, j, k, l, m, ..., drop=TRUE]
   		return(flq)
	}
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLQuant"),
	function(x, i, j, k, l, m, ..., value="missing") {
		
		if(!missing(i) && is.array(i)) {
			x@.Data[i] <- value
			return(x)
		}

		if (missing(i))
			i  <-  dimnames(x@.Data)[1][[1]]
		if (missing(j))
			j  <-  dimnames(x@.Data)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@.Data)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@.Data)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@.Data)[5][[1]]

		x@.Data[i,j,k,l,m] <- value

   		return(x)
	}
)   # }}}

## as.data.frame        {{{
if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", useAsDefault = as.data.frame)
}

setMethod("as.data.frame", signature(x="FLQuant"),
	function(x, row.names="missing", optional="missing"){
		# to avoid warnings when NA have to added
		options(warn=-1)
        if(any(is.na(suppressWarnings(as.numeric(dimnames(x)[[1]])))))
            quant <- as.factor(dimnames(x)[[1]])
        else
            quant <- as.numeric(dimnames(x)[[1]])

		df <- data.frame(expand.grid(quant=quant,
			year=as.numeric(dimnames(x)[[2]]),
			unit=as.factor(dimnames(x)[[3]]),
			season=as.factor(dimnames(x)[[4]]),
			area=as.factor(dimnames(x)[[5]])),
			data=as.vector(x))
		names(df)[1] <- quant(x)
		options(warn=0)
		return(df)
	}
)   # }}}

## apply            {{{
if (!isGeneric("apply")) {
	setGeneric("apply", function(X, MARGIN, FUN, ...){
		value  <-  standardGeneric("apply")
		value
	})
}

setMethod("apply", signature(X="FLQuant"),

	function(X, MARGIN, FUN, ...){

		data <- apply(X@.Data, MARGIN, FUN, ...)
		# set dim
		dim <- c(1,1,1,1,1)
		if (is.null(dim(data)))
			dim[MARGIN] <- length(data)
		else
			dim[MARGIN] <- dim(data)

		# new flq
		flq <- FLQuant(dim=dim, units=units(X), quant=quant(X))
		flq[1:dim[1],1:dim[2],1:dim[3],1:dim[4],1:dim[5]] <- data

		# dimnames
		dimnames <- dimnames(X)
		dimnames(flq) <- dimnames[MARGIN]

		return(flq)
	}
)   # }}}

## window           {{{
if (!isGeneric("window")) {
	setGeneric("window", useAsDefault = window)
}
setMethod("window", signature="FLQuant",
	function(x, start, end, extend=TRUE, frequency=1) {

		# get original min and max
		min <- dims(x)$minyear
		max <- dims(x)$maxyear

		if(!extend && (start < min | end > max))
			stop("FLQuant to be extended but extend=FALSE")

		# construct new FLQuant
		years <- seq(start, end, by=frequency)
		dim <- dim(x)
		dim[2] <- length(years)
		flq <- FLQuant(NA, dim=dim, units=units(x), quant=quant(x))
		# copy dimanmes ...
		dimnames(flq)[c(1,3:5)] <- dimnames(x)[c(1,3:5)]
		# ... but changing years
		dimnames(flq) <- list(year=years)

		# add data for matching years
		flq[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]  <-
			x[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]

		return(flq)
	}
)   # }}}

## trim		{{{
if (!isGeneric("trim")) {
	setGeneric("trim", function(object, ...){
		standardGeneric("trim")
		}
	)
}

setMethod("trim", signature("FLQuant"), function(object, ...){

	args <- list(...)

	c1 <- args[[quant(object)]]
	c2 <- args[["year"]]
	c3 <- args[["unit"]]
	c4 <- args[["season"]]
	c5 <- args[["area"]]

	# check if the criteria is not larger then the object
	v <- c(length(c1), length(c2), length(c3), length(c4), length(c5))
	if(sum(v > dim(object))!=0){
		stop("\n  Your criteria are wider then the object dim. I don't know what to do !\n")
	}

	if(!is.null(c1)){
		v1 <- dimnames(object)[[1]] %in% c1
		object <- object[v1,,,,, drop=FALSE]
		}
	if(!is.null(c2)){
		v2 <- dimnames(object)[[2]] %in% c2
		object <- object[,v2,,,, drop=FALSE]
		}
	if(!is.null(c3)){
		v3 <- dimnames(object)[[3]] %in% c3
		object <- object[,,v3,,, drop=FALSE]
		}
	if(!is.null(c4)){
		v4 <- dimnames(object)[[4]] %in% c4
		object <- object[,,,v4,, drop=FALSE]
		}
	if(!is.null(c5)){
		v5 <- dimnames(object)[[5]] %in% c5
		object <- object[,,,,v5, drop=FALSE]
		}
	return(object)
})	# }}}

## Sums         {{{
quantTotals <- function(x) {
	
	sums <- x
	for (i in 1:dim(x)[2])
		sums[,i,,,] <- rowSums(x, dim=2)
	return(sums)
}

yearTotals <- function(x) {
	
	sums <- x
	for (i in 1:dim(x)[1])
		sums[i,,,,] <- colSums(x)[,1,1,1]
	return(sums)
}

quantSums <- function(x, na.rm=TRUE) {
	return(apply(x, 2:5, sum, na.rm=na.rm))
}

yearSums <- function(x, na.rm=TRUE) {
	return(apply(x, c(1,3:5), sum, na.rm=na.rm))
}

unitSums <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:2,4:5), sum, na.rm=na.rm))
}

seasonSums <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:3,5), sum, na.rm=na.rm))
}

areaSums <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:4), sum, na.rm=na.rm))
}

dimSums <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:2), sum, na.rm=na.rm))
}

quantMeans <- function(x, na.rm=TRUE) {
	return(apply(x, 2:5, mean, na.rm=na.rm))
}

yearMeans <- function(x, na.rm=TRUE) {
	return(apply(x, c(1,3:5), mean, na.rm=na.rm))
}

unitMeans <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:2,4:5), mean, na.rm=na.rm))
}

seasonMeans <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:3,5), mean, na.rm=na.rm))
}

areaMeans <- function(x, na.rm=TRUE) {
	return(apply(x, c(1:4), mean, na.rm=na.rm))
}	# }}}

## torfm flq 2 formula  {{{
setGeneric("tofrm", function(object, ...){
	standardGeneric("tofrm")
	}
)

setMethod("tofrm", signature("FLQuant"), function(object, by="quant", ...){

	dn <- dimnames(object)
	dm <- dim(object)
	lst <- dn[dm>1]
	dn <- names(lst)
	if(identical(by,"quant")){
		rform <- as.list(dn[-1])
		rform$sep <- "*"
		rform <- do.call("paste", rform)
		rform <- paste(dn[1], rform, sep="|")
	}
	if(identical(by,"year")){
		rform <- as.list(dn[-2])
		rform$sep <- "*"
		rform <- do.call("paste", rform)
		rform <- paste(dn[2], rform, sep="|")
	}
	x <- paste("data", rform, sep="~")
	as.formula(x)		
}) # }}}
