# FLQuants - A list of FLQuant objects

# Author: FLR Team, Ernesto Jardim
# Additions:
# Last Change: 14 Dec 2005 13:25
# $Id: FLQuants.R,v 1.1.2.1 2005/12/19 11:22:22 iagoazti Exp $

# Reference:
# Notes:

## FLQuants     {{{

validFLQuants  <-  function(object){
	lst <- lapply(object, dim)
	lst0 <- rep(list(lst[[1]]),length(lst))
	names(lst0) <- names(lst)
	all.equal(lst0,lst)
}

setClass("FLQuants", contains="list", validity=validFLQuants)

# }}}

## FLQuants()   {{{

FLQuants <- function(...) {
	args <- list(...)
	if(length(args)==1) lst <- args[[1]]
	if(length(args)>1) lst <- args
	new("FLQuants", lst)
} 


