# FLSR - Test of FLSR class and methods

# Author: FLR Team
# Additions:
# Last Change: 09 Set 2005 10:24
# $Id: FLSR.R,v 1.7.2.3 2006/03/27 12:39:04 dorleta Exp $

# Reference:
# Notes:

library(FLCore)
data(ple4)
data(ple4.biol)

## class
fsr <- new("FLSR")
validObject(fsr)

## FLSR()
fsr <- FLSR()
fsr <- FLSR(ple4)
fsr <- FLSR(ple4.biol)
fsr <- FLSR(ple4, rec.age = 3)
fsr <- FLSR(ple4, rec.age = 3, model = "qhstk", desc = "SR data obtained from ple4")
fsr <- FLSR(ple4.biol, rec.age = 3)
fsr <- FLSR(rec = ple4@stock.n[1,])
fsr <- FLSR(rec = ple4@stock.n[1,], ssb = ssb(ple4))
fsr <- FLSR(ssb = ssb(ple4))

validObject(fsr)

## is.FLSR
is.FLSR(fsr)

## validFLSR
validObject(fsr)

## sr()
fsr <- FLSR(ple4, model="ricker")
fs1 <- sr(fsr)
fsr <- FLSR(ple4, model="bevholt")
fs2 <- sr(fsr)
fsr <- FLSR(ple4, model="segreg")
fs3 <- sr(fsr)
fsr <- FLSR(ple4, model="qhstk")
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