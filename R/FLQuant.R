# FLQuant.R - FLQuant class and methods

# Author: FLR Team
# Maintainer: Iago Mosqueira, AZTI Tecnalia
# Additions:
# Last Change: 03 abr 2006 12:45
# $Id: FLQuant.R,v 1.52.2.33 2006/05/05 11:49:48 ejardim Exp $

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
	function(object, dim="missing", ...) {
		if(missing(dim))
			dim <- c(nrow(object), ncol(object), rep(1,4))[1:5]
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

		names(x) <- tolower(names(x))
    	validnqnames <-c("year","unit","season","area")
	    validdnames <-c("data","freq")

		indices <- match(c(validnqnames,validdnames), names(x))
	   	indices <- indices[!is.na(indices)]

    	qname <- names(x)
		qname[indices] <- NA
		qname <- qname[!is.na(qname)]

    	dindices <- match(validdnames,names(x))
		dindices <- dindices[!is.na(dindices)]
		dname <- names(x)[dindices]

    	if (length(qname) > 1)
			stop("too many columns or wrong column names in data.frame")
		
		# quant
		if (!any(qname == names(x)))
			qnames <- "all"
		else {
			qnames <- as.character(sort(unique((x[, qname]))))
		}
		# year
		if (!any("year" == names(x)))
			year <- "1"
		else {
			year <- as.character(sort(unique((x[, "year"]))))
		}
		# unit
		if (!any("unit" == names(x)))
			unit <- "unique"
		else {
			unit <- as.character(sort(unique((x[, "unit"]))))
		}
		# season
		if (!any("season" == names(x)))
			season <- "all"
		else {
			season <- as.character(sort(unique((x[, "season"]))))
		}
		# area
		if (!any("area" == names(x)))
			area <- "all"
		else {
			area <- as.character(sort(unique((x[, "area"]))))
		}
		# data
		if (is.factor(x[, dname]))
			x[, dname]  <-  as.numeric(as.character(x[, dname]))
		
		flq <- FLQuant(array(x$data, dim=c(length(qnames), length(year),
        	length(unit), length(season), length(area)),
            dimnames=list(quant=qnames, year=year, unit=unit,
            season=season,  area=area)), units=units, quant=qname)

		if(!missing(dimnames))
			dimnames(flq) <- dimnames
		if(!missing(quant))
			quant(flq) <- quant

		return(flq)

		# TODO Dom 27 Nov 2005 23:22:54 GMT iagoazti: Review this section (LK)
		df <- as.data.frame(flq)
		join.on <- c(validnqnames[!is.na(match(validnqnames, names(x)))], qname)
		new.x <- x[!duplicated(x[,join.on]),]
		new.x <- merge(cbind(df,order=1:nrow(df)),new.x, all.x=TRUE, by=join.on)
		new.x <- new.x[order(new.x[, "order"]),]
		flq <- FLQuant(as.vector(new.x[,8]), dimnames=dimnames(flq))
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
	setGeneric("names", function(x){
		value  <-  standardGeneric("names")
		value
	})
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

## summary          {{{
if (!isGeneric("summary")) {
	setGeneric("summary", useAsDefault = summary)
}

setMethod("summary", signature(object="FLQuant"),
	function(object, ...){
		cat("An object of class \"FLQuant\" with:\n\n")
		dimnames(object)
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
    dots$cex <- bub.scale*(data$data/max(data$data, na.rm=T)+0.05)
    pfun <- function(x, y, ..., cex, subscripts){
        panel.xyplot(x, y, ..., cex = cex[subscripts])
        }
    call.list <- c(x = x, dots, panel=pfun)
    xyplot <- lattice::xyplot
    ans <- do.call("xyplot", call.list)
    ans$call <- match.call()
    ans

})
# }}}

## "["             {{{
setMethod("[", signature(x="FLQuant"),
	function(x, i="missing", j="missing", k="missing", l="missing", m="missing",
		..., drop="missing") {

		if (missing(i))
			#i  <-  dimnames(x@.Data)[1][[1]]
			i  <-  seq(1, length(dimnames(x@.Data)[1][[1]]))
		if (missing(j))
			j  <-  dimnames(x@.Data)[2][[1]]
   		if (missing(k))
   			k  <-  dimnames(x@.Data)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@.Data)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@.Data)[5][[1]]

   		if (missing(drop)) {
	  		flq	 <- FLQuant(x@.Data[i, j, k, l, m, drop=FALSE])
			units(flq) <- units(x)
			quant(flq) <- quant(x)
		}
		else if(drop)
             flq  <- x@.Data[i, j, k, l, m, ..., drop=TRUE]
	  	else {
		 	flq  <- FLQuant(x@.Data[i, j, k, l, m, drop=FALSE])
			units(flq) <- units(x)
			quant(flq) <- quant(x)
		}

   		return(flq)
	}
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLQuant"),
	function(x, i="missing", j="missing", k="missing", l="missing", m="missing",
		..., value="missing") {
		
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
	setGeneric("as.data.frame", function(x, row.names="missing", optional="missing"){
		value <- standardGeneric("as.data.frame")
		value
	})
}

setMethod("as.data.frame", signature(x="FLQuant", row.names="missing", optional="missing"),
	function(x, row.names, optional){
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

### flq 2 formula
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
})


