% $Id: read.FLStock.Rd,v 1.6.2.2 2005/12/20 08:16:00 iagoazti Exp $
\name{read.FLStock}
\alias{read.FLStock}
\alias{read.VPAFile}
\title{ Import stock data from a file }
\description{
  This function read stock data and create an FLStock object with it. It currently supports bot "VPA" and "ADAPT" formats
}
\usage{
read.FLStock(file, type="VPA", name, desc=paste("Imported from a", type, "file. (", file, "). ", date()), m=0.2, quant = 'age')
}
\arguments{
	\item{file}{ the file to import from (specify the 'index' file for VPA). }
	\item{type}{ either "VPA" or "Adapt", depending on the format of the data file. }
	\item{name}{ the name of this stock. }
	\item{desc}{ a short description of this stock. }
	\item{m}   { natural mortality.}
	\item{quant}{The name of the first dimension. A character string. Defaults to "quant".}
}

\value{
  An \code{FLStock} object is returned, with corresponding data
}
\author{Laurence Kell & Philippe Grosjean}

\seealso{ \code{\link{FLStock}}, \code{\link{write.FLStock}} }
\examples{
## Supposing the directory c:/mydata contains some VPA suite data, use:

# my.stock <- read.FLStock("c/mydata/index")

## to import stock data into the variable 'my.stock'
}
\keyword{ IO }
