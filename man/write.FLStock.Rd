% $Id: write.FLStock.Rd,v 1.2.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{write.FLStock}
\alias{write.FLStock}
\title{ Write FLStock data to a file }
\description{
  This function exports stock data to file (currently, only "VPA" format is supported)
}
\usage{
write.FLStock(FLStock, file, type = "VPA")
}
\arguments{
  \item{FLStock}{ An \code{FLStock} object to be exported }
  \item{file}{ The name (and path) of the file to export the data }
  \item{type}{ The type of file (currently, only "VPA") }
}

\value{
  This function returns nothing. It is used for its side effect of creating files to export stock data
}

\author{ Laurence Kell }

\seealso{ \code{\link{FLStock}}, \code{\link{read.FLStock}} }
\examples{
## If the variable 'my.stk' contains an FLStock object, then

# write.FLStock(my.stk, file="c:/mydata/index")

## creates a VPA SUITE series a files containing stock data from 'my.stk'
}
\keyword{ IO }
