# FLQuants - A list of FLQuant objects

# Author: FLR Team, Ernesto Jardim
# Maintainer: Iago Mosqueira, AZTI Tecnalia
# Additions:
# Last Change: 12 jul 2006 20:12
# $Id: FLQuants.R,v 1.1.2.9 2006/07/13 10:13:35 iagoazti Exp $

# Reference:
# Notes:

## FLQuants     {{{

setClass("FLQuants", contains="list")

# }}}

## FLQuants()   {{{

FLQuants <- function(...) {
	args <- list(...)
	if(length(args)==1 & is.list(args[[1]])) lst <- args[[1]]
	if(length(args)>1) lst <- args
	lst0 <- lapply(lst, is.FLQuant)
	if(!identical(length(lst0),sum(unlist(lst0)))) stop("All elements must be \"FLQuant\" objects.\n")
	new("FLQuants", lst)
} # }}}

## as.data.frame	{{{
setMethod("as.data.frame", signature(x="FLQuants", row.names="ANY", optional="missing"),
    function(x, row.names="col", optional) {
        # Check that all FLQuant objects' dimensions match
        dims <- matrix(unlist(lapply(x, dim)), nrow=length(x), byrow=TRUE)
        dims <- dims/matrix(dims[1,], ncol=ncol(dims), nrow=nrow(dims), byrow=T)
        if(any(dims != 1))
            stop("Dimensions of the individual objects must be the same")

        # Use first FLQuant as data.frame
        res <- as.data.frame(x[[1]])
        if(length(x) == 1)
            return(res)

        # Fill up missing names in FLQuants
        names <- names(x)
        names[names(x)==""] <- paste("data", 1:length(x), sep="")[names(x) == ""]
        
        # cbind
        if (missing(row.names)) {
           # Convert other FLQuant objects
            mat <- lapply(lapply(x[2:length(x)], as.data.frame),
                function(x) subset(x, select=data))
            res <- cbind(res, mat)
            names(res) <- c(names(res)[1:5], names)
        }
        # rbind
        else if(row.names == "row") {
            # Convert other FLQuant objects
            mat <- lapply(x[2:length(x)], as.data.frame)
            for (i in seq(1, length(mat)))
                res <- rbind(res, mat[[i]])
            res <- cbind(res,rep(names, each=dim(mat[[1]])[1]))
            names(res)[7] <- "flquant"
        }
        return(res)
    }
)	# }}}

# summary
setMethod("summary", signature("FLQuants"), function(object){

	df0 <- as.data.frame(object)
	lst <- split(df0[,c(6,7)], df0[,3:5])
	lapply(lst, function(x) apply(x,2,summary))
})

# xyplot
setMethod("xyplot", signature("formula", "FLQuants"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lnames <- names(data)
	if(is.null(lnames)){
		lnames <- paste("data", 1:length(data), sep="")
		names(lst$data)[5+1:length(data)] <- lnames	
	}
	lform <- strsplit(lnames, split=" ")
	lform$sep <- "+"
	lform <- do.call("paste", lform)
	rform <- as.list(x)[[3]]
	x <- as.formula(paste(lform, deparse(rform), sep="~"))
	lst$x <- x
	do.call("xyplot", lst)

})


# tofrm
setMethod("tofrm", signature("FLQuants"), function(object, by="quant", ...){

	flqs <- mcf(object)
	dn <- dimnames(flqs[[1]])
	dm <- dim(flqs[[1]])
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
	
	lnames <- names(object)
	if(is.null(lnames)) lnames <- paste("data", 1:length(object), sep="")
	lform <- strsplit(lnames, split=" ")
	lform$sep <- "+"
	lform <- do.call("paste", lform)

	x <- paste(lform, rform, sep="~")
	as.formula(x)		

})


## "["             {{{
setMethod("[", signature(x="FLQuants"),
	function(x, i="missing", j="missing",..., drop="missing") {

		if (missing(i))
			i  <-  seq(1, length(x))
        res <- new('FLQuants', x@.Data[i])
        names(res) <- names(x)[i]
   		return(res)
	}
)   # }}}

# make compatible flquants
setGeneric("mcf", function(object, ...){
	standardGeneric("mcf")
	}
)

setMethod("mcf", signature("list"), function(object){

	# names 
	if(!is.null(names(object))){
		flqnames <- names(object)
	} else {
		flqnames <- paste("v", 1:length(object), sep="")
	}
	# how many flquants exist ?
	v <- unlist(lapply(object, is.FLQuant))
	nflq <- sum(v)
	lst0 <- object[v]
	 
	# names and dim of the compatible flq
	dn <- dimnames(lst0[[1]])
	for(i in 1:nflq){
		dn1 <- dimnames(lst0[[i]])
		dn[[1]] <- as.character(sort(as.numeric(unique(c(dn[[1]],dn1[[1]])))))
		dn[[2]] <- as.character(sort(as.numeric(unique(c(dn[[2]],dn1[[2]])))))
		dn[[3]] <- unique(c(dn[[3]],dn1[[3]]))
		dn[[4]] <- unique(c(dn[[4]],dn1[[4]]))
		dn[[5]] <- unique(c(dn[[5]],dn1[[5]]))
	}
	dflq <- unlist(lapply(dn, length))	
	# new flquant
	flq <- FLQuant(dim=dflq, dimnames=dn)
	# preparing the list
	lst <- list()
	length(lst) <- nflq
	lst <- object
	names(lst) <- flqnames	

	# fulfiling the quants	
	for(j in 1:length(lst)){
		dn2 <- dimnames(lst[[j]])
		flq0 <- flq
		flq0[dn2[[1]], dn2[[2]], dn2[[3]], dn2[[4]], dn2[[5]]] <- lst[[j]]
		lst[[j]] <- flq0
	}
	
	# output
	FLQuants(lst)	
})

# data frame with list of values (good for xyplot)

setGeneric("data.list", function(object, ...){
	standardGeneric("data.list")
	}
)

setMethod("data.list", signature("list"), function(object, ...){

	# names 
	if(!is.null(names(object))){
		flqnames <- names(object)
	} else {
		flqnames <- paste("v", 1:length(object), sep="")
	}

	# data.frames
	flqs.lst <- lapply(object, as.data.frame)
	flqs.nlst <- lapply(flqs.lst, nrow)
	flqs.df <- do.call("rbind", flqs.lst)
	flqs.df$qname <- rep(flqnames, unlist(flqs.nlst))
	flqs.df

})

# bubbles

# bubbles
setMethod("bubbles", signature(x="formula", data ="FLQuants"), function(x, data, bub.scale=2.5, bub.col=gray(c(0.1, 0.9)), ...){

	dots <- list(...)
	data <- data.list(data)
	# def col to plot negative values
	col <- as.numeric(data$data>=0)
	coln <- vector(mode="character", length=length(col))
	# color for negs
	coln[col==0] <- bub.col[1]
	# color for pos
	coln[col==1] <- bub.col[2]
	coln[coln==""] <- NA
	dots$col <- coln

	# data
	data$data <- abs(data$data)
	dots$data <- data
	
	# bubles size to be setted by panel.function
	dots$cex <- data$data
	dots$cex <- bub.scale*dots$cex/max(dots$cex, na.rm=TRUE)+0.1*(dots$cex+1)

	# panel.function
	dots$panel <- function(x,y,..., cex, subscripts){

		dots <- list(...)
		dots$pch=19
		call.list <- dots
		call.list$x <- x
		call.list$y <- y
		call.list$cex = cex[subscripts]

		ans <- do.call("panel.xyplot", call.list)
		ans

		call.list$col <- 1
		call.list$pch <- 1
		ans <- do.call("panel.xyplot", call.list)
		ans

	}
	# call.list
	call.list <- c(x = x, dots)
	# plot
	ans <- do.call("xyplot", call.list)
	ans

})
