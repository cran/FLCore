# FLAccesors - «Short one line description»

# Author: Iago Mosqueira, AZTI Tecnalia
# Additions:
# Last Change: 28 feb 2006 17:41
# $Id: FLAccesors.R,v 1.1.2.3 2006/03/13 12:12:14 iagoazti Exp $

# Reference:
# Notes:

# TODO Mér 14 Dec 2005 16:01:45 GMT iagoazti: Improve exclusion method

## createFLAccesors		{{{
createFLAccesors <- function(object, exclude=character(1)) {

	slots <- getSlots(class(object))[!names(getSlots(class(object)))%in%exclude]

	defined <- list()

	for (x in names(slots)) {
		# check method is defined already and signatures match
		eval(
		substitute(if(isGeneric(x) && names(formals(x)) != "object") {warning(paste("Accesor method for",
			x, "conflicts with a differently defined generic. Type", x,  "for more information")); break},
			list(x=x))
			)
		# create new generic and accesor method
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x, function(object, ...) standardGeneric(x)), list(x=x))
		)
		eval(
		substitute(setMethod(x, signature(y), function(object) return(slot(object, x))), list(x=x,
			y=class(object)))
		)
		# create replacement method
		xr <- paste(x, "<-", sep="")
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x,
			function(object, value) standardGeneric(x)), list(x=xr))
		)
		eval(
		substitute(setMethod(x, signature(object=y, value=v), function(object, value)
			{slot(object, s) <- value; object}), list(x=xr, y=class(object), s=x, v=unname(slots[x])))
		)
		defined[[x]] <- c(x, xr, paste('alias{',x,',',class(object),'-method}', sep=''),
			paste('\alias{',xr,',',class(object),',',unname(slots[x]), '-method}', sep=''),
			paste('\alias{',x,'-methods}', sep=''),
			paste('\alias{"',xr, '"-methods}', sep='')
		)
	}
	return(defined)
}	# }}}
