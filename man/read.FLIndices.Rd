% $Id: read.FLIndices.Rd,v 1.2.2.1 2005/12/19 11:13:48 iagoazti Exp $
\name{read.FLIndices}
\alias{read.FLIndices}
\alias{read.FLIndex}
\title{ Import FLIndices data from a file }
\description{
  An FLIndices object (collection of indices) is initialized by importing data. Currently, only VPA format is recognized.
}
\usage{
read.FLIndices(file, file2, type="VPA", index.names, descs, methods="CPUE",
    desc = paste("Imported form ", type, " file '", file, "'", sep = ""))
}
\arguments{
  \item{file}{ The VPA CPUEs file }
  \item{file2}{ Second output file for ICA style indices}
  \item{type}{Type of index file. Presently supports "VPA", "ICA", "Adapt" and "CSA"}
  \item{index.names}{ Names for the various index datasets }
  \item{descs}{ Description for the various index datasets }
  \item{methods}{ Methods for the various index datasets. By default, "CPUE" for all series }
  \item{desc}{ A general desription for the FLIndices collection }
}

\value{
  An \code{FLIndices} object is returned
}

\author{FLR Team}

\seealso{\code{\link{FLIndices-class}}}
\examples{\dontrun{
## Supposing you have a VPA suite data in youe working directory
## and indices datasets are in the file 'fleet', then:

# my.indices <- read.Indices("fleet")

## should import these data into an FLIndices object
}}
\keyword{IO}
