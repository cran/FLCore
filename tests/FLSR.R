# FLSR - Test of FLSR class and methods

# Author: FLR Team
# Additions:
# Last Change: 09 Set 2005 10:24
# $Id: FLSR.R,v 1.7.2.4 2006/08/03 12:03:22 dorleta Exp $

# Reference:
# Notes:

library(FLCore)
data(ple4)
data(ple4.biol)

## class
fsr <- new("FLSR")

## is.FLSR
is.FLSR(fsr)

## validFLSR
validObject(fsr)


## FLSR()
fsr <- FLSR()
fsr <- FLSR(ssb = ssb(ple4))
fsr <- FLSR(rec = ple4@stock.n[1,])
fsr <- FLSR(rec = ple4@stock.n[1,], ssb = ssb(ple4))
fsr <- FLSR(params = matrix(1,1,3))
fsr <- FLSR(model = "segreg")

## sr()
fsr <- as.FLSR(ple4, model="ricker")
fs1 <- sr(fsr)
fsr <- as.FLSR(ple4, model="bevholt")
fs2 <- sr(fsr)
fsr <- as.FLSR(ple4, model="segreg")
fs3 <- sr(fsr)
fsr <- as.FLSR(ple4, model="qhstk")
fs4 <- sr(fsr)

## summary
summary(fs1)

## predict
fpe <- predict(fs1)
fpe <- predict(fs1, ssb = 200000)
fpe <- predict(fs1, ssb = c(100000, 200000))
fpe <- predict(fs2)
fpe <- predict(fs2, ssb = 200000)
fpe <- predict(fs2, ssb = c(100000, 200000))
fpe <- predict(fs3)
fpe <- predict(fs3, ssb = 200000)
fpe <- predict(fs3, ssb = c(100000, 200000))
fpe <- predict(fs4)
fpe <- predict(fs4, ssb = 200000)
fpe <- predict(fs4, ssb = c(100000, 200000))

## plot

## steepvirginbiom
fstep <- steepvirginbiom(fs1, 5)
fstep <- steepvirginbiom(fs2, 5)

## LogLikelihood.

lkhd <- srlkhd(fs1)
lkhd <- srlkhd(fs2)
lkhd <- srlkhd(fs3)
lkhd <- srlkhd(fs4)

## as.FLSR():: FLStock
fsr <- as.FLSR(ple4)
fsr <- as.FLSR(ple4, rec.age = 3)
fsr <- as.FLSR(ple4, rec.age = 3, model = "qhstk", desc = "SR data obtained from ple4")
fsr <- as.FLSR(ple4, params = matrix(c(1,2,NA),1,3))

## as.FLSR():: FLBiol
fsr <- as.FLSR(ple4.biol)
fsr <- as.FLSR(ple4.biol, rec.age = 3)
fsr <- as.FLSR(ple4.biol, rec.age = 3, model = "qhstk", desc = "SR data obtained from ple4")
fsr <- as.FLSR(ple4.biol, params = matrix(c(1,2,NA),1,3))

