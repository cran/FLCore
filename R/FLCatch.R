# FLCatch - «Short one line description»

# Author: FLR Team
# Maintainer: Jan Jaap Poos, RIVO
# Additions:
# Last Change: 30 mar 2006 19:27
# $Id: FLCatch.R,v 1.1.2.4.2.1 2007/03/29 18:43:04 janjaappoos Exp $

# Reference:
# Notes:

## validFLCatch                 {{{
validFLCatchYears <- function(object,years) {

    # FLQuants must have same number of years/ages
    Dim <- dim(object@catch.n)
    # and years need to be equal to set years if required
    if (!missing(years)) Dim[2] <- years
    # Moreover, quant has to be equal in all FLQuants
    Quant <- quant(object@catch.n)

    s. <- list("catch.n","catch.wt","catch.sel","landings.n","landings.wt",
        "landings.sel","discards.n","discards.wt","discards.sel","catchability","price")
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
}

validFLCatch <- function(object) {
    result <- validFLCatchYears(object)
    return(result)
} # }}}

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
        catchability= "FLQuant",
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
        catchability= new("FLQuant"),
        price       = new("FLQuant")
    ),
	validity=validFLCatch
)
invisible(createFLAccesors(new("FLCatch"), exclude=c('range')))	# }}}

## transform::FLCatch   {{{
setMethod("transform", signature(`_data`="FLCatch"),
	function(`_data`, ...) {
		args <- list(...)
		for (i in 1:length(args))
			slot(`_data`, names(args)[i])[,,,,] <- args[[i]][,,,,]
		return(`_data`)
	}
)   # }}}

## computeLandings	{{{
setMethod("computeLandings", signature(object="FLCatch"),
	function(object, na.rm=TRUE) {
        res <- quantSums(landings.n(object)*landings.wt(object), na.rm=na.rm)
        units(res) <- paste(units(landings.n(object)), units(landings.wt(object)))
        return(res)
 	} 
)	# }}}

## computeDiscards	{{{
setMethod("computeDiscards", signature(object="FLCatch"),
	function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        units(res) <- paste(units(discards.n(object)), units(discards.wt(object)))
        return(res)
 	} 
)	# }}}

## computeCatch	{{{
setMethod("computeCatch", signature(object="FLCatch"),
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
        }
		return(res)
    }
)	# }}}

# window::FLCatch   {{{
setMethod("window", signature="FLCatch",
  function(x, start, end, extend=TRUE, frequency=1) {
    for (s. in names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])) {
      slot(x, s.) <- window(slot(x, s.), start=start, end=end, extend=extend, frequency=frequency)
    }
    x@range["minyear"] <- start
    x@range["maxyear"] <- end

    return(x)
  }
)    # }}}

## summary::FLCatch             {{{
setMethod("summary", signature(object="FLCatch"),
    function(object, ...){
	    cat("An object of class \"FLCatch\"\n\n")
		cat("Name:", object@name, "\n")
		cat("Gear:", object@gear, "\n")
	  cat("Range:\tmin\tmax\tminyear\tmaxyear\n")
		cat("", object@range, "\n", sep="\t")
        
		for (s in names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])) {
			if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
				cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [", dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
        }
    }
) # }}}

## FLCatch()                {{{
# FLCatch  <-  FLCatch(name="character", gear="character", iniFLQuant="FLQuant", ...)
FLCatch <- function(name=character(0), gear=character(0), iniFLQuant, ...) {
    
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
        "landings.sel","discards.n","discards.wt","discards.sel","catchability",
        "price")) slot(catch, i) <- iniFLQuant

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

