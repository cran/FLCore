# FLFleet - «Short one line description»

# Author: Iago Mosqueira, AZTI Fundazioa
# Additions:
# Last Change: 26 mar 2006 19:21
# $Id: FLFleet.R,v 1.6.2.2 2006/03/28 07:19:58 iagoazti Exp $

# Reference:
# Notes:

library(FLCore)

# create one catch fleet, as in example 
catch <- FLQuant(NA, dimnames=list(age=as.character(1:12), year=as.character(c(1991:2000)),
          unit="all", season="all", area="all"))
fleet <- FLQuant(NA, dimnames=list(quant="all", year=as.character(c(1991:2000)),
          unit="all", season="all", area="all"))
my.fleet <- FLFleet(iniFLQuantFleet=fleet, iniFLQuantCatch=catch,
    catchname="examplecatch",desc="Just an examplefleet")


#create two catch fleet of two species and 1 gears
my.fleet2 <- FLFleet(iniFLQuantFleet=fleet, iniFLQuantCatch=catch,
    catchname=c("examplecatch1","examplecatch2"),
    catchstock=c("stock1","stock2"), catchgears="gear1" )


#create two catch fleet of 1 species and 2 gears
my.fleet3 <- FLFleet(iniFLQuantFleet=fleet, iniFLQuantCatch=catch,
    catchname=c("examplecatch1","examplecatch2"), 
    catchstock=("stock1"), catchgears=c("gear1","gear2"))


#create four catch fleet of 2 species and 2 gears
my.fleet4 <- FLFleet(iniFLQuantFleet=fleet, iniFLQuantCatch=catch,
    catchname=as.character(1:4), catchstock=c("stock1", "stock2", "stock1", "stock2"),     
    catchgears=c("gear1","gear1","gear2","gear2"))

#as.FLStock
stock1 <- as.FLStock(my.fleet4, name="3", gear="gear2")

#window
my.smallfleet3 <- window(my.fleet3,1995,2002)


#as.FLFleet(FLStock)
data(ple4)
coerced.fleet <- as.FLFleet(ple4)

# is FLFleet
is.FLFleet(coerced.fleet)
is.FLFleet(my.fleet2)

#summary(FLFleet)
summary(coerced.fleet)

# discards
coerced.fleet@catches[[1]]@discards.n[] <- 6
coerced.fleet@catches[[1]]@discards.wt[] <- 9
coerced.fleet@catches[[1]]@discards <- discards(coerced.fleet@catches[[1]])

# landings
coerced.fleet@catches[[1]]@landings <- landings(coerced.fleet@catches[[1]])

#catch
coerced.fleet@catches[[1]]@catch <- catch(coerced.fleet@catches[[1]]) 

# transform.
dimn <- dimnames(coerced.fleet@effort)
transformed.coerced.fleet <- transform(coerced.fleet, effort = FLQuant(c(8,6,3,2,3,2,4),dimnames=dimn), capacity=FLQuant(3,dimnames=dimn))
 
#plot
plot(transformed.coerced.fleet)

