# FLSR - Test of FLSR class and methods

# Author: FLR Team
# Additions:
# Last Change: 09 Set 2005 10:24
# $Id: FLSR.R,v 1.7.2.2 2005/12/19 11:23:49 iagoazti Exp $

# Reference:
# Notes:

library(FLCore)
data(ple4)
ple4@stock.n <- ple4@landings.n

## class
fsr <- new("FLSR")
validObject(fsr)

## FLSR()
fsr <- FLSR()
fsr <- FLSR(ple4)
validObject(fsr)

## is.FLSR
is.FLSR(fsr)

## sr()
#fs <- sr(fsr, model="ricker")
#fsr@r <- fsr@r*1000
#fs <- sr(fsr, model="bevholt")
#fs <- sr(fsr, model="qhstk")

## summary
#summary(fs)

## predict
#fpe <- predict(fsr)

## plot
