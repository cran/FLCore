library(FLCore)
dyn.load("FLCoreClasses.dll")

## Test FLquant ################################################################
flq=FLQuant(1:210,dimnames=list(age=c(1:10),year=(1980:2000)))
new.flq<-FLQuant(.Call("TestFLQuant",flq))
flq-new.flq
################################################################################

## Test FLquant2 ###############################################################
flqs<-list(FLQuant(1:210,dimnames=list(age=c(1:10), year=(1980:2000))))
new.flqs<-.Call("TestFLQuant2",flqs)

for (i in 1:length(flqs)) 
   print(new.flqs[[i]]-flqs[[i]])

flqs<-list(FLQuant(1:210,dimnames=list(age=c(1:10), year=(1980:2000))),
           FLQuant(1:30, dimnames=list(age=c(1:5),  year=(1990:1995))))

new.flqs<-.Call("TestFLQuant2",flqs)

for (i in 1:length(flqs)) 
   print(new.flqs[[i]]-flqs[[i]])
################################################################################

## Test FLBiol #################################################################
biol<-FLBiol(iniFLQuant=FLQuant(NA,dimnames=list(age=c(1:10),year=(1980:2000))))
biol@range["plusgroup"]<-biol@range["max"]
biol@m[,,,,]   <-0.2
biol@fec[,,,,] <-c(rep(0,2),.5,rep(1.0,7))
biol@wt[,,,,]  <-1:10
biol@n[,,,,]   <-2:11
biol@spwn[,,,,]<-0.0
 
new.biol<-.Call("TestFLBiol",biol)

slots<-getSlots("FLBiol")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(biol,i)-slot(new.biol,i))
  }
################################################################################

## Test FLStock ################################################################
data(ple4)
 
new.stock<-.Call("TestFLStock",ple4)

slots<-getSlots("FLStock")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(ple4,i)-slot(new.stock,i))
  }
################################################################################

## Test FLCatch ################################################################
new.catch<-.Call("TestFLCatch",north.beam@catches[[1]])
slots<-getSlots("FLCatch")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(north.beam@catches[[1]],i)-slot(new.catch,i))
  }
################################################################################

## Test FLIndex ################################################################
data(ple4.cpue)
new.index<-.Call("TestFLIndex",ple4.cpue[[1]])
slots<-getSlots("FLIndex")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(ple4.cpue[[1]],i)-slot(new.index,i))
  }
################################################################################

## Test FLIndexSurvey ##########################################################
new.index<-.Call("TestFLSurvey",ple4.indices[[1]])
slots<-getSlots("FLIndexSurvey")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(ple4.indices[[1]],i)-slot(new.index,i))
  }
################################################################################

## Test FLIndexCom #############################################################
new.index<-.Call("TestFLCom",ple4.indices[[1]])
slots<-getSlots("FLIndexCom")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(ple4.indices[[1]],i)-slot(new.index,i))
  }
################################################################################

## Test FLIndexAcoustic ########################################################
new.index<-.Call("TestFLAcoustic",ple4.indices[[1]])
slots<-getSlots("FLIndexAcoustic")
for (i in names(slots[slots=="FLQuant"])) {
  print(slot(ple4.indices[[1]],i)-slot(new.index,i))
  }
################################################################################

## Test FLFleet ################################################################
load("D:/FLR/Packages/FLCore/data/north.beam.RData")
load("D:/FLR/Packages/FLCore/data/south.beam.RData")

new.north<-.Call("TestFLFleet",north.beam)
new.south<-.Call("TestFLFleet",south.beam)
################################################################################



