# FLStock - «Short one line description»

# Author: Iago Mosqueira & Dorleta García, AZTI Fundazioa
# Additions:
# Last Change: 22 mar 2006 16:08
# $Id: FLStock.R,v 1.6.2.7 2006/03/22 16:37:58 iagoazti Exp $

# Reference:
# Notes:

library(FLCore)

## class
fls <- new("FLStock")

## FLSTock()
fls <- FLStock()
fls <- FLStock(name = 'fls', desc = 'A test')
fls <- FLStock(name = 'fls', desc = 'A test', iniFLQuant = FLQuant())
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = as.FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9), units='NA'))
fls <- FLStock(name = 'fls', desc = 'A test',
	 iniFLQuant = as.FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9), quant='age'))
fls <- FLStock(name = 'fls', desc = 'A test',
	 iniFLQuant = FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9), quant='age'))
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = FLQuant(matrix(abs(rnorm(45)), ncol=5, nrow=9)), quant='age')
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = FLQuant(array(abs(rnorm(45)), dim = c(3,7,2,1,1))), quant='length')
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = FLQuant(array(abs(rnorm(45)), dim = c(3,7,2))), quant='length')
fls <- FLStock(name = 'fls', desc = 'A test',
	 iniFLQuant = FLQuant(array(abs(rnorm(45)), dim = c(3,7,1,2))), quant='length')
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = FLQuant(array(abs(rnorm(45)), dim = c(3,7,1,1,2))), quant='length')
fls <- FLStock(name = 'fls', desc = 'A test',
	iniFLQuant = FLQuant(array(abs(rnorm(45)), dim = c(3,8,1,1,2)),
	dimnames = list(age = 1:3, year = 1993:2000, area = c("North", "South"))),
	quant='length')

## is.FLStock
is.FLStock(fls)

## landings
fls <- FLStock(iniFLQuant = FLQuant(dim = c(5, 10,1,1,1)))
fls@landings.n <- FLQuant(array(1:10,dim =c(5,10)))
fls@landings.wt <- FLQuant(array(0.5:4.5,dim =c(5,10)))
fls@landings <- landings(fls)

## discards
fls@discards.n <- FLQuant(array(1:10,dim =c(5,10)))
fls@discards.wt <- FLQuant(array(0.5:4.5,dim =c(5,10)))
fls@discards <- discards(fls)

#catch
fls@catch.n <- FLQuant(array(1:10,dim =c(5,10)))
fls@catch.wt <- FLQuant(array(0.5:4.5,dim =c(5,10)))
fls@catch <- catch(fls)

## summary
summary(fls)

## plot
fls@catch.n <- FLQuant(array(matrix(c(rnorm(10, 50000, 100),
	rnorm(10, 40000, 100), rnorm(10, 30000, 100), rnorm(10, 20000, 100),
	rnorm(10, 10000, 100)),	5, 10, byrow =T), dim = c(5, 10, 1, 1, 1)))
fls@catch.wt <- FLQuant(array(matrix(c(rnorm(10, 1, .2), rnorm(10, 2, .3),
	rnorm(10, 3, .4), rnorm(10, 3, .5), rnorm(10, 4, .6)), 5, 10, byrow = T),
	dim = c(5, 10, 1, 1, 1)))
fls@landings.n <- FLQuant(array(matrix(c(rnorm(10, 50000, 100),
	rnorm(10, 40000, 100), rnorm(10, 30000, 100), rnorm(10, 20000, 100),
	rnorm(10, 10000, 100)), 5, 10, byrow =T), dim = c(5, 10, 1, 1, 1)))*0.9
fls@discards.n <- FLQuant(array(matrix(c(rnorm(10, 50000, 100),
	rnorm(10, 40000, 100), rnorm(10, 30000, 100), rnorm(10, 20000, 100),
	rnorm(10, 10000, 100)), 5, 10, byrow =T), dim = c(5, 10, 1, 1, 1)))*0.1

fls@landings.wt <- fls@catch.wt
fls@catch <- computeCatch(fls)
fls@discards <- computeDiscards(fls)
fls@landings <- computeLandings(fls)
fls@discards.wt <- fls@catch.wt

# Los slots mat, stock.wt, stock.n y  f tienen que estar rellenos para que el
# plot "ts" funcione.

fls@mat <- FLQuant(array(c(0,0.3,0.5,0.8,1), dim = c(5,10)))
fls@stock.wt <- fls@catch.wt
fls@stock.n <- FLQuant(array(c(1e7,1e5,1e4,1e3,1e2), dim =c(5,10)))
fls@harvest <- FLQuant(array(c(1e-3,5e-3,1e-2,5e-2,1e-1), dim =c(5,10)), units="f")

fls2 <- FLStock(iniFLQuant = as.FLQuant(array(dim = c(5,10,2))))
fls3 <- FLStock(iniFLQuant = as.FLQuant(array(dim = c(5,10,2))))
fls4 <- FLStock(iniFLQuant = as.FLQuant(array(dim = c(5,10,2))))
s <- slotNames(fls2)[-(1:4)]
for(i in s){
 A <- array(dim = c(dim(slot(fls,i))[1:2],2))
 A[,,1] <- slot(fls,i)
 A[,,2] <- slot(fls,i)*rlnorm(1)
 slot(fls2, i) <- as.FLQuant(A)
 A <- array(dim = c(dim(slot(fls,i))[1:2],1,2))
 A[,,,1] <- slot(fls,i)
 A[,,,2] <- slot(fls,i)*rlnorm(1)
 slot(fls3, i) <- as.FLQuant(A)
 A <- array(dim = c(dim(slot(fls,i))[1:2],1,1,2))
 A[,,,,1] <- slot(fls,i)
 A[,,,,2] <- slot(fls,i)*rlnorm(1)
 slot(fls4, i) <- as.FLQuant(A)
}

fls2@catch <- computeCatch(fls2)
fls3@catch <- computeCatch(fls3)
fls4@catch <- computeCatch(fls4)

#plot(fls)
#plot(fls2)
#plot(fls3)
#plot(fls4)

# window

a <- window(fls, 3,5)
a <- window(fls2,2,4)
a <- window(fls,2,4, FALSE)
a <- window(fls,2,4, frequency = 2)
a <- window(fls,5,10,4, frequency = 2)

# setPlusGroup

a <- setPlusGroup(fls, 3)
a <- setPlusGroup(fls)
a <- setPlusGroup(fls,1)

# ssb
fls@m <- as.FLQuant(array(0.2, dim = c(5,10)))
fls@harvest.spwn <- as.FLQuant(array(0, dim = c(5,10)), units="f")
fls@m.spwn <- as.FLQuant(array(0, dim = c(5,10)))
ssb(fls)

# transform.
fls2 <- transform(fls, catch.n = fls@catch.n/1000, stock.n = fls@catch.n*1000)

# apply.
fls2 <- apply(fls, 2:5, mean)
fls2 <- apply(fls, 2:5, sum)
