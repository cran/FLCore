# FLQuants - A list of FLQuant objects

# Author: FLR Team, Ernesto Jardim
# Maintainer: Iago Mosqueira, AZTI Tecnalia
# Additions:
# Last Change: 26 mar 2006 18:57
# $Id: FLQuants.R,v 1.1.2.6 2006/05/05 09:00:51 ejardim Exp $

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
