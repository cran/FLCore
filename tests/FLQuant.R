# FLQuant - Tests for the FLQuant class and methods

# Author: FLR Team
# Additions:
# Last Change: 04 nov 2006 15:38
# $Id: FLQuant.R,v 1.10.2.6 2006/11/06 05:15:33 iagoazti Exp $

# Reference:
# Notes:

library(FLCore)

## class
fla <- new("FLQuant")
fla <- new("FLQuant", array(rnorm(10), dim=c(2,5,1,1,1), 
	dimnames=list(age=1:2, year=1:5, unit='unique', season='all', area='unique')))
fla <- new("FLQuant", array(rnorm(10), dim=c(2,5,1,1,1), 
	dimnames=list(age=1:2, year=10:14, unit='unique', season='all', area='unique')), units='kg')

## FLQuant
# object="missing"
fla <- FLQuant()
fla <- FLQuant(units='kg')
fla <- FLQuant(quant='length')
fla <- FLQuant(units='kg', quant='length')
fla <- FLQuant(dim=c(4,5,1,1,1))
fla <- FLQuant(dim=c(4,5,1,1,1), units='kg', quant='length')
fla <- FLQuant(dimnames=list(age=1:4, year=1:5, unit=1, season=1, area=1),
	units='kg')
# object="vector"
fla <- FLQuant(2, dim=c(4,5,2,2,2), units='kg')
fla <- FLQuant(2, dimnames=list(age=1:2, year=1:5, unit="unique",
	season=1:4, area='unique'), units='kg')
fla <- FLQuant(1:4, dim=c(6,5,1,1,1), units='kg', quant='length')
fla <- FLQuant(1:4, dimnames=list(kell=0:2, year=1990:1998,  unit='unique',
	season='all', area='unique'), units='kg')
# object="array"
fla <- FLQuant(matrix(rnorm(20,1,1), ncol=5,nrow=4))
fla <- FLQuant(matrix(rnorm(20,1,1), ncol=5,nrow=4), quant='length')
fla <- FLQuant(matrix(rnorm(20,1,1), ncol=5,nrow=4), quant='length', units='cm')
fla <- FLQuant(array(1:4, dim=c(4,3,2,2,2)))
fla <- FLQuant(array(rnorm(20,1,1), dim=c(2,5,2,1,1)))
fla <- FLQuant(array(rnorm(20,1,1), dim=c(2,5,2,1,1)), quant='length')
fla <- FLQuant(array(rnorm(20,1,1), dim=c(2,5,2,1,1)), quant='length', units='cm')
fla <- FLQuant(array(rnorm(20,1,1), dim=c(2,5,2,1,1),
	dimnames=list(cost=c('fixed', 'variable'), year=1990:1994,
	unit=c('north','south'), season='all', area='unique')))
	quant(fla) == 'cost'
# FLQuant
fla <- FLQuant(fla, units='t')
fla <- FLQuant(fla, quant='age')
fla <- FLQuant(fla, units='t', quant='age')
validObject(fla)

## as.FLQuant
# matrix
fla <- as.FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9), units='kg', quant='age')
fla <- as.FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9), units='cm', quant='length')
fla <- as.FLQuant(matrix(abs(rnorm(18)), ncol=9, nrow=2), units='cm',
    dimnames=list(length=c("10-15", "16-20")))
fla <- as.FLQuant(matrix(abs(rnorm(5)), ncol=5, nrow=1), units='kg',
    dimnames=list(age='all',year=1:5,unit='unique', season='all', area='unique'))
fla <- as.FLQuant(matrix(abs(rnorm(5)), ncol=5, nrow=1), units='kg',
    dimnames=list(age='all',year=1:5,unit='unique', season='all'))
validObject(fla)
# data.frame
df <- data.frame(age=c(1:5,1:5), year=c(rep(1980,5),rep(1981,5)),
    unit='unique', season='unique', area='all', data=abs(rnorm(10)))
dfs <- df[c(1,3:10),]
fla <- as.FLQuant(dfs, units='kg')
dfs <- rbind(df[1,], df)
fla <- as.FLQuant(dfs, units='kg')
fla <- as.FLQuant(data.frame(age=c(1:5,1:5),year=c(rep(1980,5),rep(1981,5)),
    unit='unique', season='unique', area='all', data=abs(rnorm(10))), units='pence')
fla <- as.FLQuant(data.frame(age=c(1:5,1:5),year=c(rep(1980,5),rep(1981,5)),
    unit='unique', data=abs(rnorm(10))), units='pence')
fla <- as.FLQuant(data.frame(age=c(1:5,1:5),year=c(rep(1980,5),rep(1982,5)),
    unit='unique', data=abs(rnorm(10))), units='pence')
# numeric
fla <- as.FLQuant(seq(78,88), dimnames=list(age=1, year=1:11, unit='unique',
    season='unique', area='all'), units='kg')
validObject(fla)
# array
fla <- as.FLQuant(array(abs(rnorm(9)), dim=c(3,3,1,1,1)), units='kg')
fla <- as.FLQuant(array(abs(rnorm(168)), dim=c(7,12,1,1,2)), units='kg')
fla <- as.FLQuant(array(abs(rnorm(168)), dim=c(7,12,1,1,2)), units='kg', quant='age')
fla <- as.FLQuant(array(abs(rnorm(168)), dim=c(7,12,1,1,2),
	dimnames=list(length=seq(10,70,by=10), year=1991:2002, unit='stock',
	season='all', area=1:2)), units='kg')
fla <- as.FLQuant(array(abs(rnorm(168)), dim=c(7,12,1,1,2)),
	dimnames=list(length=seq(10,70,by=10), year=1991:2002, unit='stock',
	season='all', area=1:2), units='kg')
validObject(fla)

## quant
quant(fla)
quant(fla) <- 'length'

## units
units(fla) <- 't'
units(fla) == 't'

## dimnames
dimnames(fla)
dimnames(fla) <- list(length=seq(10, 70, by=10))
dimnames(fla) <- list(length=seq(10, 70, by=10), unit='north')

## is.FLQuant
is.FLQuant(fla)

## show
show(fla)

## summary
summary(fla)

## dims
dims(fla)

## names
names(fla)

# plot
plot(fla)

## "["
fla[1,,,,]
fla[1,'2001',,,]
fla['10','2001',,,]
fla['10',1:10,1,1]
fla['10',1:10,1,1,2]
fla[,,,,2]
fla[,'2001']
fla['20','2001']
fla[c('20','30'),'2001',,,]
fla[,10:12,1,1]
fla[,as.character(1991:2001),,,]
fla['10',,,,'1']
fla[1:3,5:12,,,'1']
fla[as.character(c(10,20,30)),5:12,,,'1']

## "[<-"
fla[1,,,,] <- seq(1, 1.4, length=12)
fla[1,'2001',,,] <- fla[1,'2001',,,] + 0.5

## as.data.frame
flf <- as.data.frame(fla)

## apply
apply(FLQuant(array(1, dim=c(4,3,2,2,2)), quant='age'), 1:3, sum)
apply(FLQuant(array(1, dim=c(4,3,2,2,2)), quant='age'), 1, sum)
apply(FLQuant(array(1, dim=c(4,3,2,2,2)), quant='age'), 2:4, sum)

## window
fla <- window(fla, 1992, 2001)
fla <- window(fla, 1990, 2000)
fla <- window(fla, 1992, 2002, frequency=2)
fla <- window(fla, 1990, 2002)
## subset

## aggregate
