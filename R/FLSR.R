# FLSR.R - FLSR class and methods for S/R data

# Author: FLR Team
# Maintainer: Richard Hillary
# Additions:
# Last Change: 19 Dec 2005 23:44
# $Id: FLSR.R,v 1.50.2.6 2005/12/20 08:16:00 iagoazti Exp $

# Reference:
# Notes:

## class :: FLSR                   {{{
setClass("FLSR",
	representation(
 		name     = "character",
		desc     = "character",
		model    = "character",
		ssb      = "FLQuant",
		rec      = "FLQuant",
		rechat   = "FLQuant",
		residuals= "FLQuant",
		params   = "matrix",
		se       = "numeric",
		covar    = "matrix",
		var      = "numeric",
		varacorr = "numeric",
		aic      = "numeric",
#        call     = "call",
        mcmc     = "list"),
	prototype=prototype(
		name     = character(0),
		desc     = character(0),
		model    = "NULL",
		ssb      = new("FLQuant"), 
		rec      = new("FLQuant"),
		rechat   = new("FLQuant"),
		residuals= new("FLQuant"),
		params   = matrix(NA, ncol=3, dimnames=list("", c('alpha','beta','rho'))),
        se       = unlist(list(alpha=as.double(NA), beta=as.double(NA), rho=as.double(NA))),
        covar    = matrix(rep(NA, 9), nrow=3, ncol=3, 
            		dimnames = list(c("alpha", "beta", "rho"), c("alpha", "beta", "rho"))),
        var      = as.double(NA),
		varacorr = as.double(NA),
		aic      = as.double(NA),
#        call     = NULL,
        mcmc     = list(params=list(),
				pinit=unlist(list(alpha=as.double(NA),beta=as.double(NA),S0=as.double(NA),
				h=as.double(NA),rho=as.double(NA),sigma=as.double(NA))),
				S0.mean=as.double(NA),S0.var=as.double(NA),h.alpha=as.double(NA),
				h.beta=as.double(NA),logalp.mean=as.double(NA),
				logalp.var=as.double(NA),beta.mean=as.double(NA),beta.var=as.double(NA),
				rho.alp=as.double(NA),rho.bet=as.double(NA),
				beta.min=as.double(NA),beta.max=as.double(NA),sigma.shape=as.double(NA),
				sigma.rate=as.double(NA),nIter=as.integer(NA),thin=as.integer(NA),
				nChains=as.integer(NA),burnin=as.integer(NA),SSBperRec=as.double(NA),MCvar=as.vector(NA)))
)

# TODO 20/2/2005 iago :: write validity function.
#   - decide on dimensions of FLQuants
#setValidity("FLSR", validFLSR)
#remove(validFLSR)	
# }}}

## FLSR()                   {{{
FLSR <- function(rec="missing", ssb="missing", rec.age="missing", ...) {

    # rec = FLQuant
    if (is.FLQuant(rec) && missing(ssb)) {
        
        sr <- new("FLSR")

        slot(sr, "ssb") <- FLQuant(rec)
        slot(sr, "rec") <- FLQuant(rec)
        slot(sr, "rechat") <- FLQuant(rec)
        slot(sr, "residuals") <- FLQuant(rec)
    }

    # rec = FLQuant & ssb = FLQuant
    else if (is.FLQuant(rec) && is.FLQuant(ssb)) {

        sr <- new("FLSR")

        slot(sr, "ssb") <- ssb
        slot(sr, "rec") <- rec
        slot(sr, "rechat") <- FLQuant(rec)
        slot(sr, "residuals") <- FLQuant(rec)
    }

    # rec = FLStock
    else if (is.FLStock(rec)) {

    	sr <- new("FLSR")

    	# recruitment delay set using minage
		# from the FLStock object

		if(missing(rec.age))
			rec.age <- rec@range[['min']]
    
        if (sum(complete.cases(slot(rec, "stock.n")))  == 0 |
            sum(complete.cases(slot(rec, "stock.wt")))  == 0 |
            sum(complete.cases(slot(rec, "mat")))  == 0  | 
	    sum(complete.cases(slot(rec, "harvest.spwn"))) == 0 | 
	    sum(complete.cases(slot(rec, "m.spwn"))) == 0)
                stop("stock must have 'stock.n', 'stock.wt', 'harvest.spwn', 'm.spwn' and 'mat'")

        # calculate ssb and create FLSR object incorprating rec.age
        
	if(missing(rec.age)) {
		.rec <- rec@stock.n[1,]
	} else {
		if(rec.age < rec@range[['min']])
			stop("Supplied recruitment age less than minimum age class")
			diffrec <- rec.age-rec@range[['min']]	
			.rec <- rec@stock.n[1+diffrec,] 
	}
		 
	if(units(slot(rec,"harvest")) != 'f' && units(slot(rec,"harvest")) != 'hr')
		stop("Incorrect units specified in harvest slot of FLStock object")
	if(units(slot(rec, "harvest")) == 'f')
		.ssb <- apply(slot(rec, "stock.n") * exp(-harvest(rec, "f") *
       		slot(rec,"harvest.spwn")-slot(rec, "m")*
		slot(rec, "m.spwn"))*slot(rec, "stock.wt")*
		slot(rec, "mat"), 2:5, sum)
	if(units(slot(rec, "harvest")) == 'hr')
		.ssb <- apply(slot(rec, "stock.n") * (1 - slot(rec, "harvest") * slot(rec, "harvest.spwn")) *
		exp(-slot(rec, "m") * slot(rec, "m.spwn")) * slot(rec, "mat") * slot(rec, "stock.wt"), 2:5, sum)	


		# now alter the stock and recruitment 
		# vectors to factor in the recruitement age
	
	if((length(.rec[,,1,1,1])-1) <= rec.age)
		stop("FLStock recruitment data set too short")
		
	.rec <- .rec[,(1+rec.age):length(.rec[,,1,1,1]),,,]
	.ssb <- .ssb[,1:(length(.ssb[,,1,1,1])-rec.age),,,]
	if(length(.rec[,,1,1,1]) != length(.ssb[,,1,1,1]))
		stop("FLStock stock-recruit data not of same length")

	# create the FLSR object

	sr <- new("FLSR", rec=.rec,ssb=.ssb) 
        units(slot(sr, "rec")) <- units(slot(rec, "stock.n"))
	units(slot(sr, "ssb")) <- units(slot(rec, "stock.wt")) 

        slot(sr, "rechat") <- FLQuant(slot(sr, "rec"))
        units(slot(sr, "rechat")) <- units(slot(sr, "rec"))
        
        slot(sr, "residuals") <- FLQuant(slot(sr, "rec"))
	units(slot(sr, "residuals")) <- units(slot(sr, "rec")) 

    }

    else {
        sr <- new("FLSR")
    }
    
    # add other specified slots
    args <- list(...)
    if(length(args) > 0) {
        for (i in 1:length(args)) {
            try(slot(sr, names(args)[i]) <- args[[i]], silent=TRUE)
        }
    }
    return(sr)
} # }}}

## is.FLSR()                  {{{
is.FLSR <- function(x) {
	return(inherits(x,"FLSR"))
} # }}}

## summary :: FLSR              {{{
setMethod("summary", signature(object="FLSR"),
	function(object, ...){
		cat("An object of class \"FLSR\" with:\n\n")
		cat("Name: ", slot(object, "name"), "\n")
		cat("Desc: ", slot(object, "desc"), "\n")
        cat("SSB: [", dim(slot(object, "ssb")),"], units=", slot(object, "ssb")@units, "\n") 
        cat("Rec: [", dim(slot(object, "rec")),"], units=", slot(object, "rec")@units, "\n") 
		cat("-----\n")
		cat("Model applied:", slot(object, "model"), "\n")
		cat("Parameter estimates:\n")
		if(nrow(slot(object, "params")) > 1)
			show(apply(slot(object, "params"), 2, median))
		else
			show(slot(object, "params"))
		cat("AIC: ", slot(object, "aic"), "\n")
        if(length(object@mcmc$params) > 0)
            cat("McMC chains: ", length(object@mcmc$params), "\n")
        # TODO 20/2/2005 iago :: Add other slots?
	}
) # }}}

## predict :: Generic           {{{
if (!isGeneric("predict")) {
    setGeneric("predict", function(object, ...){
	    value <- standardGeneric("predict")
	    value
    })
} # }}}	

## predict :: FLSR              {{{
# TODO Fix for McMC output, with FLQuant(6)
setMethod("predict", signature(object="FLSR"),
	function(object, ssb="missing",...){

      		if (missing(ssb))
         		ssb <- object@ssb

		# ricker
		PredictRicker <- function(object)
			object@params[,"alpha"] * ssb * exp(-object@params[,"beta"] * ssb)

		# bevholt
		Predictbevholt <- function(object)
			object@params[,"alpha"] * ssb / (ssb + object@params[,"beta"])

		# segreg
		Predictsegreg <- function(object) {
			.retval <- vector(length=length(ssb))
			for(i in 1:length(ssb)) {
				if(ssb[i] <= object@params[,"beta"])
					.retval[i] <- object@params[,"alpha"]*ssb[i]
				if(ssb[i] > object@params[,"beta"])
					.retval[i] <- object@params[,"alpha"]*object@params[,"beta"]
			}
			return(.retval)
		}

		Predictqhstk <- function(object) {
			.retval <- vector(length=length(ssb))
			for(i in 1:length(ssb)) {
				if(ssb[i] <= object@params[[,"beta"]]*(1-object@params[[,"rho"]]))
					.retval[i] <- object@params[[,"alpha"]]*ssb[i]
				if(ssb[i] > object@params[[,"beta"]]*(1-object@params[[,"rho"]]) && ssb[i] < object@params[[,"beta"]]*(1+object@params[[,"rho"]]))
					.retval[i] <- object@params[[,"alpha"]]*(ssb[i]-(ssb[i]-object@params[[,"beta"]]*(1-object@params[[,"rho"]]))*(ssb[i]-object@params[[,"beta"]]*(1-object@params[[,"rho"]]))/(as.double(4)*object@params[[,"beta"]]*object@params[[,"rho"]]))
				if(ssb[i] >= object@params[[,"beta"]]*(1+object@params[[,"rho"]]))
					.retval[i] <- object@params[[,"alpha"]]*object@params[[,"beta"]]

			}
			return(.retval)
		}
		 
		# Main part of the predict method
		if(length(grep('ricker', object@model)) != 0) 
			res <- PredictRicker(object)
		if(length(grep('bevholt', object@model)) != 0) 
			res <- Predictbevholt(object) 
		if(length(grep('segreg', object@model)) != 0) 
			res <- Predictsegreg(object) 
		if(length(grep('qhstk', object@model)) != 0) 
			res <- Predictqhstk(object) 

		return(res)
	}
) # }}}

## plot :: FLSR                 {{{
setMethod("plot", signature(x="FLSR", y="missing"),
    function(x, y, type=c("all", "SR", "QQ", "Res.AR1", "Res.t", "Res.S", "Res.R"),
             cols=c(1, 2, 8), ...) {
	        
		if(is.na(x@aic)) {

			# check initial parameter estimate is here

			if(x@model == "bevholt" | x@model == "ricker" | x@model == "segreg") {
				if(is.na(x@params[,'alpha']) | is.na(x@params[,'beta']))
					stop("Initial parameter estimates missing")
			} else {
				if(is.na(x@params[,'alpha']) | is.na(x@params[,'beta']) | is.na(x@params[,'rho']))
					stop("Initial parameter estimates missing")	
			}
			
			ssb <-x@ssb[ 1,,,,,drop=TRUE]
			r   <-x@rec[   1,,,,,drop=TRUE] 
			year<-as.integer(dimnames(x@rec)$year)
			plot(ssb, r, xlim=c(0, max(ssb)+1), ylim=c(0, max(r)+1), xlab="SSB",
	               	ylab="Recruits", main="Initial guess S-R relationship")
			ssb <- 0:100 * max(ssb) * 0.012
			lines(ssb, as.vector(predict(x,ssb)),col="red") 

		} else {
        	ssb <-x@ssb[ 1,,,,,drop=TRUE]
			r   <-x@rec[   1,,,,,drop=TRUE]
        	rhat<-x@rechat[1,,,,,drop=TRUE]
        	year<-as.integer(dimnames(x@rec)$year)

        	res <- log(r / rhat)
        		  
		#S-R plot with fitted curve
		SRPlot <- function(ssb, r) {
			plot(ssb, r, xlim=c(0, max(ssb)+1), ylim=c(0, max(r)+1), xlab="SSB",
                ylab="Recruits", main="Stock Recruit Plot", col=cols[1])
			ssb <- 0:100 * max(ssb) * 0.012
			lines(ssb, as.vector(predict(x,ssb)), col=cols[2])
			invisible()
		}

		#Residuals at time t Vs. residuals at time t+1
		SRPlotAR1 <- function(res) {
			plot(res[1:(length(res)-1)], res[2:length(res)],
				xlab="Residuals at time t", ylab="Residuals at time t+1",
				main="AR(1) Residuals", col=cols[1])

			res.lm <- lm(as.vector(res[1:(length(res)-1)]) ~ as.vector(res[2:length(res)]))
			abline(res.lm, col=cols[2])
			#key(text=list(paste("R-sq = ", round(summary(res.lm)$r.squared, 4) * 100, "%", sep="")))
			invisible()
		}

		#Residuals plotted against year
		SRPlotResidualsByYear <- function(year,res){
			plot(year, res, xlab="Year", ylab="Residuals", main="Residuals by year",
                col=cols[1])
			lines(lowess(year, res), col=cols[2])
			lines(c(min(year), max(year)), c(0, 0), col=cols[3], lty=2)
			invisible()
		}

		#Residuals plotted against SSB
		SRPlotResidualsByX <- function(ssb,res){
			plot(ssb, res, xlab="SSB", ylab="Residuals", main="Residuals by SSB",
                col=cols[1])
			lines(lowess(ssb, res), col=cols[2])
			lines(c(min(ssb), max(ssb)), c(0, 0), col=cols[3], lty=2)
			invisible()
		}

		#Residuals plotted against Recruits
		SRPlotResidualsByYHat <- function(rhat,res){
			plot(rhat, res, xlab="R Hat", ylab="Residuals",
                main="Residuals by Recruits Hat", col=cols[1])
			lines(lowess(rhat, res), col=cols[2])
			lines(c(min(rhat), max(rhat)), c(0, 0), col=cols[3], lty=2)
			invisible()
		}

		#QQplot of residuals
		SRPlotQQ <- function(res){
			.res <- (res - mean(res)) / (var(res)^0.5)
			qqnorm(.res, col=cols[1])
			qqline(.res, col=cols[2])
			#qq <- qq95ci()
			#lines(qq[,1], qq[,2], col=cols[3], lty=2)
			#lines(qq[,1], qq[,3], col=cols[3], lty=2)
			invisible()
		}


		#All plots arranged on a single page
		SRPlots <- function(ssb,r,rhat,year,res){
			par(mfrow=c(3, 2))
			SRPlot(ssb,r)
			SRPlotResidualsByYear(year,res)
			SRPlotAR1(res)
			SRPlotResidualsByX(ssb,res)
			SRPlotQQ(res)
			SRPlotResidualsByYHat(rhat,res)
			invisible()
		}

		# The main part of the plot method
		opar <- par(no.readonly = TRUE)
		on.exit(par(opar))
		par(mfrow=c(1, 1))
		switch(as.character(type[1]),
			"all"=SRPlots(ssb,r,rhat,year,res),
			"SR"=SRPlot(ssb,r),
			"QQ"=SRPlotQQ(res),
			"Res.AR1"=SRPlotAR1(res),
			"Res.t"=SRPlotResidualsByYear(year,res),
			"Res.S"=SRPlotResidualsByX(ssb,res),
			"Res.R"=SRPlotResidualsByYHat(rhat,res),
			stop("type must be 'all', 'SR', 'QQ', 'Res.AR1', 'Res.t', 'Res.S' or 'Res.R'!"))
        		invisible()
		}
	}
) # }}}

## residuals		{{{
if (!isGeneric("params")) {
	setGeneric("params", function(object, ...){
		value  <-  standardGeneric("params")
		value
	})
}

setMethod("params", signature(object="FLSR"),
	function(object, param=NULL, out=median, ...)
		if(nrow(object@params) == 1)
			return(object@params)
		else
			if(!missing(out))
				return(apply(object@params, 2, out, ...))
			else
				return(object@params)
) # }}}

## params<-		{{{
if (!isGeneric("params<-")) {
	setGeneric("params<-", function(object, ..., value){
		value  <-  standardGeneric("params<-")
		value
	})
}

#setMethod("params<-", signature(object="FLSR", value="matrix"),
#    function(object, value) {
#        object@params <- matrix(value, ncol=3, dimnames=list(1:nrow(value), c('alpha','beta','rho')))
#        return(object)
#    }
#)

setMethod("params<-", signature(object="FLSR", value="vector"),
	function(object, param=NULL, value) {
		if(length(value) != nrow(object@params))
			stop("Length of input vector does not match that of the params slot")
		object@params[,param] <- value
		if(nrow(object@params) == 1)
			dimnames(object@params)[[1]] <- ""
		return(object)
	}
) # }}}

## steepvirginbiom			{{{
# simple method to calculate steepness and virgin biomass 
# from alpha and beta in B-H model and the Ricker model: 
# SSB per unit recruit required, results returned in a simple list

steepvirginbiom <- function(object,ssbperunitrec) {
		
		if(!is.FLSR(object))
			stop("Object is not an FLSR object")
		if(!ssbperunitrec)
			stop("SSB per unit recruit missing in steepvirginbiom method")
		if(is.FLQuant(ssbperunitrec))
			ssbperunitrec <- as.double(ssbperunitrec)
		if(!(slot(object, "model") == "bevholt" | slot(object, "model") == "ricker"))
			stop("FLSR object model not Beverton-Holt or Ricker")
	
		.alpha <- object@params[,'alpha']
		.beta <- object@params[,'beta']

		# B-H model
	
		if(slot(object, "model") == "bevholt") {
			.h <- .alpha*ssbperunitrec/(4*.beta+.alpha*ssbperunitrec)
			.S0 <- (ssbperunitrec*.alpha*(5*.h-1))/(4*.h)
		}
		
		# Ricker model

		if(slot(object, "model") == "ricker") {
			.h <- 0.2*(ssbperunitrec*.alpha)^0.8
			.S0 <- 4*log(ssbperunitrec*.alpha)/.beta
		} 	
		
		res <- list(steepness=.h,virginbiomass=.S0)
		return(res)
}	# }}}

## srlkhd		{{{
# Simple function that computes the log-likelihood for a given 
# model and parameter estimates: takes FLSR object with parameter estimates 
# and a defined model.

srlkhd <- function(x) {
	
	# input checks on both model and parameter estimates

	if(is.null(x@model))
		stop("FLSR object has no model")
		
	if(x@model == "bevholt" | x@model == "ricker" | x@model == "segreg") {
		if(is.na(x@params[,'alpha',]) | is.na(x@params[,'beta']))
			stop("Initial parameter estimates missing")
	} else {
		if(is.na(x@params[,'alpha']) | is.na(x@params[,'beta']) | is.na(x@params[,'rho']))
			stop("Initial parameter estimates missing")	
	}

	# compute the log-likelihood for the given model
	
	.ssb <- as.vector(x@ssb)
	.r <- as.vector(x@rec)
	.rhat <- predict(x,.ssb)
	.var <- var(log(.r/.rhat))
	 
	logL <- 0
	for(i in 1:length(.r)) {
		logL <- logL + dnorm(log(.r[i]),log(.rhat[i]),sqrt(.var),TRUE)
	} 

	# Output :: text and results

	print(x@model)
	print(x@params)
	print(logL)
}	# }}}

## ricker = Simple MLE estimate for Ricker parameters    {{{
# by recasting it as a linear system.

# Author: Richard Hillary, Imperial College
#         Iago Mosqueira, AZTI Fundazioa 

# Notes:

ricker <- function(rec, ssb, params = "missing") {

	# create results list - ToDo

	# input checks
	if (!is.vector(rec) | !is.vector(ssb))
		return(paste("Inputs not vectors"))

	# create the output list

	res <- list(rechat=as.vector(NA),
		    residuals=as.vector(NA),
			params = matrix(NA, ncol=3, dimnames=list("", c('alpha','beta','rho'))),
		    se=unlist(list(alpha=as.double(NA),beta=as.double(NA),rho=as.double(NA))),
		    covar=matrix(rep(NA,9),nrow=3,ncol=3,dimnames=list(c("alpha","beta","rho"),
                c("alpha","beta","rho"))),
		    var=as.double(NA),
		    varacorr=as.double(NA),
		    aic=as.double(NA)) 

	# perform the MLE

	T <- length(ssb)
	x <- ssb
	y <- log(rec/ssb)
	sx <- sum(x)
	sy <- sum(y)
	sxx <- sum(x*x)
	sxy <- sum(x*y)
	s2x <- sx*sx
	sxsy <- sx*sy

	.beta <- -(T*sxy-sxsy)/(T*sxx-s2x)
	.alpha <- sy/T + .beta*(sx/T)
	.alpha <- exp(.alpha)

	#  AIC calculation

	.rhat <- .alpha*ssb*exp(-.beta*ssb)
	.var <- var(log(rec)-log(.rhat))
	logL <- 0
	for(i in 1:length(rec)) {
		logL <- logL + dnorm(log(rec[i]),log(.rhat[i]),sqrt(.var),TRUE)
	}
	.aic <- -2*logL+3
	
	# calculate the variance and the autocorrelation-corrected variance

	res$rechat <- .rhat
	res$residuals <- log(rec)-log(.rhat)
	res$params[,'alpha'] <- .alpha
	res$params[,'beta'] <- .beta
	res$var <- .var
	acorr <- acf(res$residuals,plot=FALSE)$acf[2]
	res$varacorr <- .var*(1-acorr*acorr)

	# calculate the covariance matrix: direct estimate 
	# from inverse of the hessian of the log(lkhd) at the 
	# MLE

	covar <- matrix(nrow=2,ncol=2)
	covar[1,1] <- length(rec)/(.alpha*.alpha*.var)
	covar[1,2] <- -sum(ssb)/(.alpha*.var)
	covar[2,1] <- covar[1,2]
	covar[2,2] <- sum(ssb*ssb)/.var

	# invert this to get the variance-covariance matrix

	covar <- solve(covar)
	res$covar[1:2,1:2] <- covar[1:2,1:2] 
	res$se['alpha'] <- sqrt(covar[1,1])
	res$se['beta'] <- sqrt(covar[2,2])
	res$aic <- .aic 
	
	return(res)
} # }}}

# bevholt = Simple MLE estimate for Beverton-Holt (alpha,beta,sigma)  {{{
# parameters

# Author: Richard Hillary, Imperial College
#         Iago Mosqueira, AZTI Fundazioa 

# Notes:

bevholt <- function(rec, ssb, params = "missing") {

	# input checks
	
	if (!is.vector(rec) | !is.vector(ssb))
		return(paste("Inputs not vectors"))

	# create the output list

	res <- list(rechat=as.vector(NA),
		    residuals=as.vector(NA),
			params = matrix(NA, ncol=3, dimnames=list("", c('alpha','beta','rho'))),
		    se=unlist(list(alpha=as.double(NA),beta=as.double(NA),rho=as.double(NA))),
		    covar=matrix(rep(NA,9),nrow=3,ncol=3,dimnames=list(c("alpha","beta","rho"),
                c("alpha","beta","rho"))),
		    var=as.double(NA),
		    varacorr=as.double(NA),
		    aic=as.double(NA))

	# create the predicted recruitment vector

	.rhat <- vector(length=length(rec))

	# perform the MLE using the optim() algorithm

	# first, define the function to be optimised

	fn <- function(x) {
		.alpha <- x[1]
		.beta <- x[2]
		.sigma <- x[3]
		.rhat <- x[1]*ssb/(x[2]+ssb)
		logL <- 0
		for(i in 1:length(rec)) {
			logL <- logL - dnorm(log(rec[i]),log(.rhat[i]),sqrt(.sigma),TRUE)
		}
	}

	# setting up the initial guess and the bounds... dirty 
	# version: alpha = max(rec)+0.1*(max(rec)-min(rec), beta = 0.5*min(ssb)
        param <- numeric(3)
    	lower <- numeric(3)
	upper <- numeric(3)

	if(!missing(params)) {
		if(is.na(params[[1]]) | is.na(params[[2]])) { 
    			param[1] <- max(rec)+0.1*(max(rec)-min(rec))
			param[2] <- 0.5*min(ssb)
			.rhat <- param[1]*ssb/(param[2]+ssb)
			param[3] <- var(log(rec/.rhat)) 
		} else {
			param[1] <- params[[1]]
			param[2] <- params[[2]]
			.rhat <- param[1]*ssb/(param[2]+ssb)
			param[3] <- var(log(rec/.rhat))
		}
	} else {
		param[1] <- max(rec)+0.1*(max(rec)-min(rec))
		param[2] <- 0.5*min(ssb)
		.rhat <- param[1]*ssb/(param[2]+ssb)
		param[3] <- var(log(rec/.rhat)) 	
	}
		
    	lower[1] <- 0.0
    	upper[1] <- Inf
    	lower[2] <- 0.001
    	upper[2] <- Inf
    	lower[3] <- 0.001
    	upper[3] <- Inf

	resop <- optim(param,fn,
			   lower = lower,
			   upper = upper,
			   method = "L-BFGS-B",
			   hessian = TRUE,
			   control = list(trace=1))
	
	#  AIC calculation

	.rhat <- resop$par[1]*ssb/(resop$par[2]+ssb)
	logL <- 0
	for(i in 1:length(rec)) {
		logL <- logL + dnorm(log(rec[i]),log(.rhat[i]),sqrt(resop$par[3]),TRUE)
	}
	.aic <- -2*logL+3

	# return the results in the res list

	res$rhat <- resop$par[1]*ssb/(resop$par[2]+ssb)
	res$residuals <- log(rec)-log(res$rhat)
	res$params[,'alpha'] <- resop$par[1]
	res$params[,'beta'] <- resop$par[2]
	res$var <- resop$par[3]
	acorr <- acf(res$residuals,plot=FALSE)$acf[2]
	res$varacorr <- resop$par[3]*(1-acorr*acorr)

	# calculate covariance matrix from the Hessian:
	# covar = inv(Hessian) if Hessian is non-singular

	if(det(resop$hessian) != 0) {
		covar <- matrix(ncol=2,nrow=2)
		covar[1:2,1:2] <- resop$hessian[1:2,1:2]
		covar <- solve(covar)
		res$covar[1:2,1:2] <- covar[1:2,1:2]
		res$se['alpha'] <- sqrt(covar[1,1])
		res$se['beta'] <- sqrt(covar[2,2])
	} else print("Hessian singular - no covariance estimate possible")
	res$aic <- .aic

	return(res)
} # }}}

# segreg = Simple MLE estimate for the hockey-stick/segmented regression  {{{
# (alpha,beta,sigma) parameters

# Author: Richard Hillary, Imperial College
#         Iago Mosqueira, AZTI Fundazioa 

# Notes:

segreg <- function(rec,ssb, params = "missing") {

	# input checks
	
	if (!is.vector(rec) | !is.vector(ssb))
		return(paste("Inputs not vectors"))

	# create the output list

	res <- list(rechat=as.vector(NA),
		    residuals=as.vector(NA),
			params = matrix(NA, ncol=3, dimnames=list("", c('alpha','beta','rho'))),
		    se=unlist(list(alpha=as.double(NA),beta=as.double(NA),rho=as.double(NA))),
		    covar=matrix(rep(NA,9),nrow=3,ncol=3,dimnames=list(c("alpha","beta","rho"),
                c("alpha","beta","rho"))),
		    var=as.double(NA),
		    varacorr=as.double(NA),
		    aic=as.double(NA))

	# create the predicted recruitment vector

	.rhat <- vector(length=length(rec))

	# perform the MLE using the optim() algorithm
	# first, define the function to be optimised

	fn <- function(x) {
		.alpha <- x[1]
		.beta <- x[2]
		.sigma <- x[3]
		for(i in 1:length(rec)) {
			if(ssb[i] <= .beta)
				.rhat[i] <- .alpha*ssb[i]
			if(ssb[i] > .beta)
				.rhat[i] <- .alpha*.beta
		}
		logL <- 0
		for(i in 1:length(rec)) {
			logL <- logL - dnorm(log(rec[i]),log(.rhat[i]),sqrt(.sigma),TRUE)
		}
	}

	# setting up the initial guess and the bounds... 

	param <- numeric(3)
	lower <- numeric(3)
	upper <- numeric(3)

	if(!missing(params)) {
		if(is.na(params[[1]]) | is.na(params[[2]])) { 
			param[1] <- mean(rec/ssb)
			param[2] <- mean(ssb)
			for(i in 1:length(rec)) {
				if(ssb[i] <= param[2])
					.rhat[i] <- param[1]*ssb[i]
				if(ssb[i] > param[2])
					.rhat[i] <- param[1]*param[2]
			}  
			param[3] <- var(log(rec/.rhat))
		} else {
			param[1] <- params[[1]]
			param[2] <- params[[2]]
			for(i in 1:length(rec)) {
				if(ssb[i] <= param[2])
					.rhat[i] <- param[1]*ssb[i]
				if(ssb[i] > param[2])
					.rhat[i] <- param[1]*param[2]
			} 
			param[3] <- var(log(rec/.rhat))
		}
	} else {
		param[1] <- mean(rec/ssb)
		param[2] <- mean(ssb)
		for(i in 1:length(rec)) {
			if(ssb[i] <= param[2])
				.rhat[i] <- param[1]*ssb[i]
			if(ssb[i] > param[2])
				.rhat[i] <- param[1]*param[2]
		}  
		param[3] <- var(log(rec/.rhat)) 
	}

	lower[1] <- 0.001
	upper[1] <- Inf
	lower[2] <- 0.001
	upper[2] <- Inf
	lower[3] <- 0.001
	upper[3] <- Inf
	
	resop <- optim(param,fn,
			   lower = lower,
			   upper = upper,
			   method = "L-BFGS-B",
			   hessian = TRUE,
			   control = list(trace=1))
	
	# AIC calculation
	
	for(i in 1:length(rec)) {
			if(ssb[i] <= resop$par[2]) 
				.rhat[i] <- resop$par[1]*ssb[i]
			if(ssb[i] > resop$par[2])
				.rhat[i] <- resop$par[1]*resop$par[2]
	}
	logL <- 0
	for(i in 1:length(rec)) {
		logL <- logL + dnorm(log(rec[i]),log(.rhat[i]),sqrt(resop$par[3]),TRUE)
	}
	.aic <- -2*logL+3

	# return the results in the res list

	res$rechat <- .rhat
	res$residuals <- log(rec)-log(res$rhat)
	res$params[,'alpha'] <- resop$par[1]
	res$params[,'beta'] <- resop$par[2] 
	res$var <- resop$par[3]
	acorr <- acf(res$residuals,plot=FALSE)$acf[2]
	res$varacorr <- resop$par[3]*(1-acorr*acorr)

	# calculate covariance matrix from the Hessian:
	# covar = inv(Hessian)/length(rec)

	if(det(resop$hessian) != 0.0) {
		covar <- matrix(ncol=2,nrow=2)
		covar[1:2,1:2] <- resop$hessian[1:2,1:2]
		covar <- solve(covar)
		res$covar <- covar
		res$se['alpha'] <- sqrt(covar[1,1])
		res$se['beta'] <- sqrt(covar[2,2])
	} else print("Hessian singular - no covariance estimates")
	
	res$aic <- .aic
	return(res)
} # }}}

# qhstk = Simple MLE estimate for the quadratic hockey-stick    {{{
# (alpha,beta,rho,sigma) parameters

# Author: Richard Hillary, Imperial College
#         Iago Mosqueira, AZTI Fundazioa 

# Notes:

qhstk <- function(rec, ssb, params = "missing") {

	# input checks
	
	if (!is.vector(rec) | !is.vector(ssb))
		return(paste("Inputs not vectors"))

	# create the output list

	res <- list(rechat=as.vector(NA),
		    residuals=as.vector(NA),
			params = matrix(NA, ncol=3, dimnames=list("", c('alpha','beta','rho'))),
		    se=unlist(list(alpha=as.double(NA),beta=as.double(NA),rho=as.double(NA))),
		    covar=matrix(rep(NA,9),nrow=3,ncol=3,dimnames=list(c("alpha","beta","rho"),c("alpha","beta","rho"))),
		    var=as.double(NA),
		    varacorr=as.double(NA),
		    aic=as.double(NA))

	# create the predicted recruitment vector

	.rhat <- vector(length=length(rec))

	# perform the MLE using the optim() algorithm

	# first, define the function to be optimised

	fn <- function(x) {
		.alpha <- x[1]
		.beta <- x[2]
		.rho <- x[3]
		.sigma <- x[4]
		for(i in 1:length(rec)) {
			if(ssb[i] <= .beta*(1-.rho)) 
				.rhat[i] <- .alpha*ssb[i]
			if(ssb[i] > .beta*(1-.rho) && ssb[i] < .beta*(1+.rho))
				.rhat[i] <- .alpha*(ssb[i]-(ssb[i]-.beta*(1-.rho))*(ssb[i]-.beta*(1-.rho))/(as.double(4)*.rho*.beta))
			if(ssb[i] >= .beta*(1+.rho))
				.rhat[i] <- .alpha*.beta
		}
		logL <- 0
		for(i in 1:length(rec)) {
			logL <- logL - dnorm(log(rec[i]),log(.rhat[i]),sqrt(.sigma),TRUE)
		}
	}

	# setting up the initial guess and the bounds... 

	param <- numeric(4)
	lower <- numeric(4)
	upper <- numeric(4)

	if(!missing(params)) {
		if(is.na(params[[1]]) | is.na(params[[2]]) | is.na(params[[3]]) | missing(params)) {
			param[1] <- mean(rec/ssb)
			param[2] <- mean(ssb)
			param[3] <- as.double(0.5)
			for(i in 1:length(rec)) {
				if(ssb[i] <= param[2]*(1-param[3])) 
					.rhat[i] <- param[1]*ssb[i]
				if(ssb[i] > param[2]*(1-param[3]) && ssb[i] < param[2]*(1+param[3]))
					.rhat[i] <- param[1]*(ssb[i]-(ssb[i]-param[2]*(1-param[3]))*(ssb[i]-param[2]*(1-param[3]))/(as.double(4)*param[3]*param[2]))
				if(ssb[i] >= param[2]*(1+param[3]))
					.rhat[i] <- param[1]*param[2]
			}
			param[4] <- var(log(rec/.rhat))
		} else {
			param[1] <- params[[1]]
			param[2] <- params[[2]]
			param[3] <- params[[3]]
			for(i in 1:length(rec)) {
				if(ssb[i] <= param[2]*(1-param[3])) 
					.rhat[i] <- param[1]*ssb[i]
				if(ssb[i] > param[2]*(1-param[3]) && ssb[i] < param[2]*(1+param[3]))
					.rhat[i] <- param[1]*(ssb[i]-(ssb[i]-param[2]*(1-param[3]))*(ssb[i]-param[2]*(1-param[3]))/(as.double(4)*param[3]*param[2]))
				if(ssb[i] >= param[2]*(1+param[3]))
					.rhat[i] <- param[1]*param[2]
			}
			param[4] <- var(log(rec/.rhat)) 

		}
	} else {
		param[1] <- mean(rec/ssb)
		param[2] <- mean(ssb)
		param[3] <- as.double(0.5)
		for(i in 1:length(rec)) {
			if(ssb[i] <= param[2]*(1-param[3])) 
				.rhat[i] <- param[1]*ssb[i]
			if(ssb[i] > param[2]*(1-param[3]) && ssb[i] < param[2]*(1+param[3]))
				.rhat[i] <- param[1]*(ssb[i]-(ssb[i]-param[2]*(1-param[3]))*(ssb[i]-param[2]*(1-param[3]))/(as.double(4)*param[3]*param[2]))
			if(ssb[i] >= param[2]*(1+param[3]))
				.rhat[i] <- param[1]*param[2]
		}
		param[4] <- var(log(rec/.rhat))  
	}
	
	lower[1] <- 0.001
	upper[1] <- Inf
	lower[2] <- 0.001
	upper[2] <- Inf
	lower[3] <- 0.001
	upper[3] <- 0.999
	lower[4] <- 0.001
	upper[4] <- Inf
	
	resop <- optim(param,fn,
			   lower = lower,
			   upper = upper,
			   method = "L-BFGS-B",
			   hessian = TRUE,
			   control = list(trace=1))
	
	# AIC calculation
	
	for(i in 1:length(rec)) {
			if(ssb[i] <= resop$par[2]*(1-resop$par[3])) 
				.rhat[i] <- resop$par[1]*ssb[i]
			if(ssb[i] > resop$par[2]*(1-resop$par[3]) && ssb[i] < resop$par[2]*(1+resop$par[3]))
				.rhat[i] <- resop$par[1]*(ssb[i]-(ssb[i]-resop$par[2]*(1-resop$par[3]))*(ssb[i]-resop$par[2]*(1-resop$par[3]))/(as.double(4)*resop$par[3]*resop$par[2]))
			if(ssb[i] >= resop$par[2]*(1+resop$par[3]))
				.rhat[i] <- resop$par[1]*resop$par[2]
	}
	logL <- 0
	for(i in 1:length(rec)) {
		logL <- logL + dnorm(log(rec[i]),log(.rhat[i]),sqrt(resop$par[4]),TRUE)
	}
	.aic <- -2*logL+4

	# return the results in the res list

	res$rechat <- .rhat
	res$residuals <- log(rec)-log(res$rhat)
	res$params[,'alpha'] <- resop$par[1]
	res$params[,'beta'] <- resop$par[2]
	res$params[,'rho'] <- resop$par[3]
	res$var <- resop$par[4]
	acorr <- acf(res$residuals,plot=FALSE)$acf[2]
	res$varacorr <- resop$par[4]*(1-acorr*acorr)

	# calculate covariance matrix from the Hessian:
	# covar = inv(Hessian) if Hessian non-singular

	if(det(resop$hessian) != 0) { 
		covar <- matrix(ncol=3,nrow=3)
		covar[1:3,1:3] <- resop$hessian[1:3,1:3]
		covar <- solve(covar)
		res$covar <- covar
		res$se['alpha'] <- sqrt(covar[1,1])
		res$se['beta'] <- sqrt(covar[2,2])
		res$se['rho'] <- sqrt(covar[3,3])
	} else print("Hessian singular - no covariance estimate possible")
	res$aic <- .aic

	return(res)
} # }}}

## sr()       {{{
sr <- function(data, ...) {
    
    
    sr <- data
    model <- sr@model
    params <- sr@params
    mclist <- sr@mcmc

    #slot(sr, "call") <- match.call()

    # is model Bayesian or Evil?
    #if(length(grep('Bayes', model)) != 0) {
        #if(missing(data@mcmc) | !is.list(data@mcmc))
            #stop("A Bayesian model needs a list of priors")
    #}

    # extract data
    if (is.FLSR(data)) {
        rec <- as.vector(data@rec)
        ssb <- as.vector(data@ssb)
    } else
        stop("data must an FLSR object")
    
    # call selected model

    if(length(grep('Bayes', model)) == 0) {
    	call <- c(list(as.symbol(model), rec=rec, ssb=ssb, params))
    	res <- try(eval(as.call(call)), silent=TRUE)
	#res <- as.symbol(model)(rec,ssb)
    } else {
	call <- c(list(as.symbol(model), rec=rec, ssb=ssb, mclist))
 	res <- try(eval(as.call(call)), silent=TRUE)
    }
    #print(res)

    # catch error
    if(inherits(res, "try-error")) {
        stop(res)
    }

    # output for MLE fit
    if(length(grep('Bayes', model)) == 0) {
    	for(i in 1:length(res)) {
        		if(is.FLQuant(slot(sr, names(res)[i]))) {
				
				# create temporary FLQuants from res and then 
				# assign them to the relevant slots in sr object
				
				.units <- slot(sr, names(res)[i])@units
				.nm  <- dimnames(slot(sr, names(res)[i]))
				tmpflq <- FLQuant(quant="age",res[[i]],dim=c(1,length(res[[i]]),1,1,1),dimnames=.nm,units=.units)
        			slot(sr, names(res)[i]) <- tmpflq
			} else {
            			try(slot(sr, names(res)[i]) <- res[[i]], silent=TRUE)
			}
    		}
    }
     
    # ToDo: output for Bayes estimation

    if(length(grep('Bayes', model)) != 0) {

    	# return Markov chains to relevant MCMC slots

	sr@mcmc$params <- res

	# now use the mean and covariance to fill out the other FLSR object slots: check just one chain
	# ToDo
    }
	
    return(sr)
}    # }}}

## Accesors
# var accesor to overload var in stats
if (!isGeneric("var")) {
    setGeneric("var", function(x, y, na.rm, use){
	    value <- standardGeneric("var")
	    value
    })
}
setMethod("var", signature(x="FLSR", y="missing", na.rm="missing", use="missing"),
	function(x)
		return(slot(x, "var"))
)
if (!isGeneric("var<-")) {
    setGeneric("var<-", function(object, value){
	    value <- standardGeneric("var<-")
	    value
    })
}
setMethod("var<-", signature(object="FLSR", value="numeric"),
	function(object, value) {
		slot(object, "var") <- value
		return(object)
	}
)

# automatic accesors, excluding var and range
invisible(createFLAccesors(new("FLSR"), exclude=c('range', 'var')))
