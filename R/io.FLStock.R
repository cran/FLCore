# io.FLStock.R - read and write assessment input files into an FLStock

# Author: FLR Team
# Additions:
# Last Change: 14 Dec 2005 19:19
# $Id: io.FLStock.R,v 1.14.2.3 2005/12/19 11:22:23 iagoazti Exp $
# Reference:
# Notes:


## read.FLStock		{{{

read.FLStock <- function (file, type = "VPA", name, desc = paste("Imported from a", 
    type, "file. (", file, "). ", date()), m = 0.2, quant="age")
	{
    ow <- options()$warn
    options(warn = -1)
    on.exit(options(warn = ow))

    res <- switch(type,
                  VPA = read.VPA(file),
                  Adapt = read.AdaptFile(file,m),
                  PA = read.PAFile(file),
                  CSA = read.CSAFile(file),
                  stop("type must be either 'VPA', 'Adapt', 'PA' or 'CSA'!"))
    Mat <- res@stock.wt
    Mat[, , , , ] <- NA
    Dim <- dim(Mat)
    Mat0 <- Mat
    Mat0[, , , , ] <- 0
    if (is.null(res@landings.n)  || !all(dim(res@landings.n)  == Dim)) 
        res@landings.n  <- Mat
    if (is.null(res@landings.wt) || !all(dim(res@landings.wt) == Dim)) 
        res@landings.wt <- Mat
    if (is.null(res@catch.n)     || !all(dim(res@catch.n)     == Dim)) 
        res@catch.n     <- Mat
    if (is.null(res@catch.wt)    || !all(dim(res@catch.wt)    == Dim)) 
        res@catch.wt    <- Mat
    if (is.null(res@discards.n)  || !all(dim(res@discards.n)  == Dim)) 
        res@discards.n  <- Mat
    if (is.null(res@discards.wt) || !all(dim(res@discards.wt) == Dim)) 
        res@discards.wt <- Mat
    if (is.null(res@m)           || !all(dim(res@m)           == Dim)) 
        res@m           <- Mat
    if (is.null(res@stock.wt)    || !all(dim(res@stock.wt)    == Dim)) 
        res@stock.wt    <- Mat
    if (is.null(res@mat)         || !all(dim(res@mat)         == Dim)) 
        res@mat         <- Mat
    if (is.null(res@catch.wt)    || !all(dim(res@catch.wt)    == Dim)) 
        res@catch.wt    <- Mat
    if (is.null(res@stock.n)     || !all(dim(res@stock.n)     == Dim)) 
        res@stock.n     <- Mat
    if (is.null(res@harvest)           || !all(dim(res@harvest)     == Dim)) 
        res@harvest     <- Mat
    if (is.null(res@harvest.spwn)      || !all(dim(res@harvest.spwn)== Dim)) 
        res@harvest.spwn<- Mat
    if (is.null(res@m.spwn)      || !all(dim(res@m.spwn)      == Dim)) 
        res@m.spwn      <- Mat
    Mat <- Mat[1, , , , ]
#    dimnames(Mat)[1] <- "all"
    if (is.null(res@catch) || !all(dim(res@catch) == dim(Mat))) 
        res@catch <- Mat
    if (is.null(res@discards) || !all(dim(res@discards) == dim(Mat)))
        res@discards <- Mat
    if (is.null(res@landings) || !all(dim(res@landings) == dim(Mat)))
        res@landings <- Mat
    pars <- dims(res@stock.wt)
    res@range <- unlist(list(min = pars$min, max = pars$max,
        plusgroup = NA, minyear = pars$minyear, maxyear = pars$maxyear))
    if (length(res@name) < 1 | !missing(name))
        res@name <- as.character(name)
    if (!is.null(desc)) 
        res@desc <- as.character(desc)
        
    names. <- names(getSlots(class(res))[getSlots(class(res))=="FLQuant"])

    for (s. in names.) {
        quant(slot(res, s.)) <- quant
    }
    
    return(res)
}	# }}}

## read.AdaptFile	{{{
read.AdaptFile <- function(file., m. = m) {
    skip.hash <- function(i) {
        i <- i + 1
        while (substr(scan(file., skip = i, nlines = 1, what = ("character"),
            quiet = TRUE)[1], 1, 1) == "#") i <- i + 1
        return(i)
    }

    skip.until.minuFLStock.1 <- function(i) {
        i <- i + 1
        while (scan(file., skip = i, nlines = 1, what = ("character"),
            quiet = TRUE)[1] != "-1") i <- i + 1
        return(i)
    }

    FLStock. <- FLStock()
    i <- skip.hash(0)
    FLStock.@range[c("minyear", "maxyear")] <- scan(file.,
        skip = i, nlines = 1, nmax = 2, quiet = TRUE)

    i <- skip.hash(i)
    FLStock.@range[c("min", "max", "plusgroup")] <- scan(file.,
        skip = i, nlines = 1, nmax = 3, quiet = TRUE)
        
    i <- skip.hash(i) + 1
    t. <- scan(file., skip = i, nlines = 1, nmax = 1, quiet = TRUE)

    dims   <- c(FLStock.@range["max"] -FLStock.@range["min"] +1,
                FLStock.@range["maxyear"]-FLStock.@range["minyear"]+1,
                1,1,1)
#   you need a second dims object because sometimes the year labels are also read
    dims2  <- c(dims[1]+1, dims[2])
    dimnms <- list(age  =as.character(FLStock.@range["min"] :FLStock.@range["max"]),
                   year =as.character(FLStock.@range["minyear"]:FLStock.@range["maxyear"]),
                   unit = "unique",
                   season = "all",
                   area = "unique")
    nages  <- FLStock.@range["max"]  - FLStock.@range["min"]  + 1
    nyears <- FLStock.@range["maxyear"] - FLStock.@range["minyear"] + 1

    FLStock.@harvest.spwn <- as.FLQuant(array(t./12, dim = dims, dimnames = dimnms), units="f")
    FLStock.@m.spwn <- FLStock.@harvest.spwn

    i <- skip.hash(i)
    t. <- scan(file., skip = i, nlines = 1, nmax = nages, quiet = TRUE)
    FLStock.@mat <- as.FLQuant(array(rep(t., nyears), dim = dims, dimnames = dimnms))

    i <- skip.hash(i)
    FLStock.@name <- scan(file., skip = i, nlines = 1, nmax = 1,
        quiet = TRUE, what = "character")

    i <- skip.hash(i)
    t. <- scan(file., skip = i, nmax = nyears * (nages+1), quiet = TRUE)
    FLStock.@landings.n <- as.FLQuant(array(t., dim=dims2)[-1,], dimnames=dimnms)

    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    j <- skip.until.minuFLStock.1(i)
    k <- skip.hash(i)
    if (j == k) {
        i <- skip.hash(j)
        t. <- scan(file., skip = i, nmax = nyears*(nages+1), quiet = TRUE)
        FLStock.@stock.wt <- as.FLQuant(array(t., dim = dims2)[-1,], dimnames=dimnms)
    }
    else {
        i <- k
        t. <- scan(file., skip = i, nmax = nyears*(nages+1), quiet = TRUE)
        FLStock.@landings.wt <- as.FLQuant(array(t., dim = dims2)[-1,], dimnames=dimnms)

        i <- skip.until.minuFLStock.1(i)
        i <- skip.hash(i)
        t. <- scan(file., skip = i, nmax = nyears * (nages+1), quiet = TRUE)

        FLStock.@stock.wt <- as.FLQuant(array(t., dim=dims2)[-1,], dimnames=dimnms)
    }
    FLStock.@m <- as.FLQuant(array(m., dim = dims, dimnames=dimnms))
    return(FLStock.)
}	# }}}

## read.CSAFile		{{{
read.CSAFile <- function(file.) {
    
    t.    <-scan(file=file.,skip=1,sep=",")
    nrow. <-length(t.)/9
    t.    <-t(array(t.,dim=c(9,nrow.)))
    t.    <-array(t.[,-1],dim=c(20,8),
        dimnames=list(t.[,1],c("m","c.rec","c.full","w.rec","w.full","s.rat","u.rec","u.full")))

    s. <- FLStock(iniFLQuant=as.FLQuant(t(array(cbind(t.[,"c.rec"],t.[,"c.full"]),
        dim=c(nrow.,2)))))
    s.@catch.n <- as.FLQuant(t(array(cbind(t.[,"c.rec"],t.[,"c.full"]),
        dim=c(nrow.,2),
        dimnames=list(dimnames(t.)[[1]],c("r","full")))))
    s.@stock.wt <- as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]),
        dim=c(nrow.,2),
        dimnames=list(dimnames(t.)[[1]],c("r","full")))))
    s.@catch.wt <- as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]),
        dim=c(nrow.,2),
        dimnames=list(dimnames(t.)[[1]],c("r","full")))))
    s.@m <- as.FLQuant(t(array(t.[,"m"], 
        dim=c(nrow.,2),
        dimnames=list(dimnames(t.)[[1]],c("r","full")))))

    mat.0 <- FLQuant(0,dimnames(s.@catch.n))
    mat.na <- FLQuant(NA,dimnames(s.@catch.n))

    s.@harvest.spwn <- mat.0
    s.@m.spwn <- mat.0

    s.@landings.n <- s.@catch.n
    s.@landings.wt <- s.@catch.wt
    s.@discards.n <- mat.0
    s.@discards.wt <- mat.0

    d.            <- dimnames(s.@catch.n)
    d.$age        <- "all"
    s.@catch      <- as.FLQuant(apply(s.@catch.wt   *s.@catch.n,   2,sum),d.)
    s.@landings   <- as.FLQuant(apply(s.@landings.wt*s.@landings.n,2,sum),d.)
    s.@discards   <- as.FLQuant(apply(s.@discards.wt*s.@discards.n,2,sum),d.)

    s.@mat        <-mat.na
    s.@stock.n    <-mat.na
    s.@harvest    <-FLQuant(NA,dimnames(s.@catch.n), units='f')

    s.@range["minyear"]     <-min(t.[,1])   
    s.@range["maxyear"]     <-max(t.[,1])   

    s.@desc		<-"read in from CSA file"

    return(s.)
}	# }}}
    
## read.PAFile		{{{
read.PAFile <- function(file.) {
    getmatrix <- function(file., start, nlines, yrs, ages) {
        m. <- t(as.matrix(read.table(file = file., skip = start - 
            1, row.names = 1, nrows = nlines, sep = ",",colClasses = "numeric")[, 
            (ages[1]:ages[2]) + 1 - ages[1]]))
        return(as.FLQuant(m. <- array(m., dim = c(ages[2] - 
            ages[1] + 1, yrs[2] - yrs[1] + 1), dimnames = list(ages[1]:ages[2], 
            yrs[1]:yrs[2]))))
    }
    range <- scan(file., skip = 2, nlines = 2, sep = ",")
    ages <- range[4:5]
    yrs <- range[1:2]
    FLStock. <- FLStock()
    FLStock.@m.spwn <- getmatrix(file., 6, yrs[2] - yrs[1] + 
        1, yrs, ages)
    FLStock.@harvest.spwn <- getmatrix(file., 7 + (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@harvest <- getmatrix(file., 7 + 3 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
		harvest(FLStock.) <- "f"
    FLStock.@stock.wt <- getmatrix(file., 7 + 4 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@catch.wt <- getmatrix(file., 7 + 5 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@mat <- getmatrix(file., 7 + 6 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@m <- getmatrix(file., 7 + 7 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    yrs[2] <- yrs[2] + 1
    FLStock.@stock.n <- getmatrix(file., 6 + 2 * (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 2, yrs, ages)
    FLStock.@range <- c(minage = ages[1], maxage = ages[2], 
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- "read in from PA file"
    FLStock.@name <- scan(file., nlines = 1, what = character(0))[1]
    return(FLStock.)
}	# }}}

## read.VPAFile		{{{
read.VPAFile <- function(file, sep = "", units = "NA") {	
    if (!file.exists(file)) stop(paste("The file ", file, "does not exist"))

    switch (as.character(file.access(file)),
        "0"  = info <- read.table(file, colClasses = "character", header = FALSE, fill = TRUE, skip = 1, nrows = 4, sep = sep),
        "-1" = info <- matrix(rep("0", 8), nrow = 4, ncol = 2))

    misc <- info[1, 1]
    type <- info[1, 2]
    dfor <- info[4, 1]

    # Switch for file type (dfor; e.g. matrix, scalar, vector)
    switch(misc, "1" = {
                        range <- scan(file, skip = 2, nlines = 2, sep = sep, quiet=TRUE)
                        ages <- range[3:4]
                        nages <- ages[2] - ages[1] + 1
                        yrs <- range[1:2]
                        nyrs <- yrs[2] - yrs[1] + 1

                        dms <- list(age=as.character(ages[1]:ages[2]),year=as.character(yrs[1]:yrs[2]))
                        switch(dfor,
              	                    "1" = a. <- as.FLQuant(matrix(t(read.table(file = file, skip = 5, nrows = nyrs, sep = sep)[, 1:nages]), nrow=nages, ncol=nyrs),dimnames= dms),
                                    "2" = a. <- as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep, quiet=T)[1:nages], nyrs), nrow = nages, ncol = nyrs), dimnames = dms),
                                    "3" = a. <- as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep, quiet=T)[1], nyrs * nages),nrow = nages, ncol = nyrs), dimnames = dms),
                                    "5" = {
                                           dms <- list(age="all",year=as.character(yrs[1]:yrs[2]))
                                           a. <- as.FLQuant(matrix(t(read.table(file = file, skip = 5, nrows = nyrs, sep = sep)[,1]), nrow = 1, ncol = nyrs), dimnames = dms)
                                           })
                        a. <-  FLQuant(as.numeric(a.),dimnames=dimnames(a.))  #needed to go from int to double
                        return(a.)
          },
       "0" = cat("Invalid file. Cannot read file:-", file, "\n"),
       cat("Tuning file", file, "not read", "\n")
    )
}	# }}}

## read.VPA		{{{
read.VPA <- function(file, sep = "") {
    if (!file.exists(file))
        stop(paste("VPA index file", file, "does not exist"))

    dir <- dirname(file)
    files. <- scan(file, what = "character", skip = 2, sep = sep, quiet=TRUE)
    files. <- file.path(dir, files., fsep = .Platform$file.sep)

    range <- scan(files.[1], skip = 2, nlines = 2, sep = sep, quiet=TRUE)
    ages <- range[3:4]
    yrs <- range[1:2]

    FLStock. <- FLStock(iniFLQuant=FLQuant(NA, dimnames = list(age = ages[1]:ages[2], year = yrs[1]:yrs[2], unit = "unique", season = "all",  area = "unique")))

    for (i in files.) {
        if (!file.exists(i)) cat("File ", i, "does not exist", "\n")
        else {
            a.   <-  read.VPAFile(i)

            switch(as.character(scan(i, skip = 1, nlines = 1, sep = sep, quiet=TRUE)[2]),
            "1" = FLStock.@landings    <-a.,
            "2" = FLStock.@landings.n  <-a.,
            "3" = FLStock.@landings.wt <-a.,
            "4" = FLStock.@stock.wt    <-a.,
            "5" = FLStock.@m           <-a.,
            "6" = FLStock.@mat         <-a.,
            "7" = FLStock.@harvest.spwn<-a.,
            "8" = FLStock.@m.spwn      <-a.,
            "21"= FLStock.@discards    <-a.,
            "22"= FLStock.@discards.n  <-a.,
            "23"= FLStock.@discards.wt <-a.,
            "24"= FLStock.@catch       <-a.,
            "25"= FLStock.@catch.n     <-a.,
            "26"= FLStock.@catch.wt    <-a.,
            "27"= FLStock.@harvest     <-a.,
            "28"= FLStock.@stock.n     <-a. )
        }
    }

    FLStock.@range <- c(min = ages[1], max = ages[2],
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- paste("Imported from a VPA file (",
        file, "). ", date(), sep="")
    FLStock.@name <- scan(file, nlines = 1, what = character(0),
        sep = "\n", quiet=TRUE)

    return(FLStock.)
}	# }}}

## write.FLStock	{{{

write.FLStock <- function(FLStock, file, type="VPA") {
	if (!inherits(FLStock, "FLStock"))
		stop("FLStock must be an 'FLStock' object!")
	switch(type,
    # TODO 22/12/2004 iagoazti : doesn't use file as base filename
		"VPA" = write.VPA(FLStock, dirname(file), species=FLStock@name),
		stop("type must be 'VPA'!"))
}	# }}}

# write.VPA - David Bromley, CEFAS		{{{

write.VPA <- function(FLStock, dir=getwd(), species=FLStock@name) {

    #provides names for data files
    names <- c(paste(species,"CN.txt",sep=""), paste(species,"CW.txt",sep=""),
        paste(species,"SW.txt",sep=""), paste(species,"NM.txt",sep=""),
        paste(species,"MO.txt",sep=""), paste(species,"PF.txt",sep=""),
        paste(species,"PM.txt",sep=""))

    # provides the index numbers for each VPA file
    index <- c(2,3,4,5,6,7,8)								

    # provides the descriptions for each VPA file
    desc	<- c("catch-at-age (thousandes)", "catch weight-at-age (kg)", 	
        "stock weight-at-age", "natural mortality", " maturity-at-age ogive", 
        "proportion of F before spawning", "proportion of M before spawning")

    #calculates the numbers of ages
    nage  <- FLStock@range[2]-FLStock@range[1]+1
    #calculates the number of years
    nyear <- FLStock@range[5]-FLStock@range[4]+1

    for(i in 1:length(names)) {
        # open the output file connections
        temp <- file(file.path(dir, names[i], fsep = .Platform$file.sep), "w")
        # adds the VPA format info to the begining of each file
        cat(FLStock@name, ":", desc[i], "\n", file=temp)
        cat(1, index[i],"\n", file=temp, sep="\t")	
        cat(FLStock@range[4], FLStock@range[5], "\n", file=temp, sep="\t")
        cat(FLStock@range[1], FLStock@range[2], "\n", file=temp, sep="\t")
        cat(1,"\n",  file=temp)
        x <- as.character(i)
            # appends the data to each file
		    switch(x,
			"1"= write(matrix(FLStock@catch, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"2"= write(matrix(FLStock@cwt, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"3"= write(matrix(FLStock@swt, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"4"= write(matrix(FLStock@m, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"5"= write(matrix(FLStock@mat, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"6"= write(matrix(FLStock@harvest.spwn, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),
			"7"= write(matrix(FLStock@m.spwn, nrow=nage, ncol=nyear), ncolumns=nage, file=temp),)
         close(temp)
    }

    # produces the index file
    temp <- file(file.path(dir, paste(species, "IND.txt", sep=""), fsep = .Platform$file.sep), "w")
	cat(FLStock@name, "\n", file=temp)
	cat(1,names, file=temp, sep="\n")
	close(temp)
}	# }}}
