# FLIndex - «Short one line description»

# Author: Iago Mosqueira, AZTI Fundazioa
# Additions:
# Last Change: 06 mar 2006 15:56
# $Id: FLIndex.R,v 1.5.2.3 2006/03/13 12:12:14 iagoazti Exp $

# Reference:
# Notes:

library(FLCore)

#
fi <- new("FLIndex")

# FLIndex()
fi <- FLIndex()
fi <- FLIndex(name = 'index')
fi <- FLIndex(name = 'index', desc = 'An index')
fi <- FLIndex(index=FLQuant(1:10))
fi <- FLIndex(index=FLQuant(dim=c(5,10,1,1,1)))

# is.FLIndex
is.FLIndex(fi)

# summary
summary(fi)

# dims
dims(fi)

# window
fw <- window(fi, start=1, end=15, frequency=3)
fw <- window(fw, start=1, end=5)

# apply
fi <- apply(fi, list('index'), sqrt)

# transform
fi <- transform(fi, index=fi@index*2)

# accesors
name(fi)
name(fi) <- 'fi'
