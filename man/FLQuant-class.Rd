% $Id: FLQuant-class.Rd,v 1.9.2.5 2006/06/14 11:49:40 ejardim Exp $
\name{FLQuant}
\docType{class}
\alias{FLQuant-class}
\alias{show,FLQuant-method}
\alias{[,FLQuant-method}
\alias{[<-,FLQuant-method}
\alias{summary,FLQuant-method}
\alias{quant,FLQuant-method}
\alias{quant<-,FLQuant-method}
\alias{plot,FLQuant,missing-method}
\alias{names,FLQuant-method}
\alias{dimnames<-,FLQuant-method}
\alias{trim,FLQuant-method}
\alias{catch,FLQuant-method}
\alias{tofrm,FLQuant-method}
\alias{discards,FLQuant-method}
\alias{landings,FLQuant-method}
%\alias{aggregate,FLQuant-method}
\alias{apply,FLQuant,ANY,ANY-method}
\alias{as.data.frame,FLQuant,ANY,ANY-method}

\title{FLQuant class and methods}

\description{FLQuant is the basic structure for age, time, and area structured fisheries
	information in the FLR library. Its basic structure is a five-dimensional array, with
	named dimensions and an 'units' attribute to store the measurement units of the data.
	
	The name of the first dimension of an FLQuant is not set, and can be altered by the
	user. This is meant to reflect the first dimension the data is structured along. This
	could be age, length, vesselclass,... Any character string is accepted, but it should
	contain no spaces. When not set it is usually refered to as the quant of the FLQuant.
	The other four dimensions of an FLQuant are, in this order, year, unit, season, and
	area. The first one needs no explanation, the second is open to any sort of division
	that might be of use, like male/female or substocks. The last two allow for space and
	time subdivisions. Dimensions names are always of class character, even if they represent
	numeric values.
	
	Together with the data, FLQuants have an extra attribute, called units This is a character
	string to store information about the units of measurement of the data. Do not confuse
	it with the unit dimension. Users are allowed to use any name, but standard ones are
	encouraged as they allow for clear interpretation.}

\section{Objects from the Class}{
  Objects are usually created by calls to the constructor function
  \code{\link{FLQuant}}.  They also can be created by calls of the form
  \code{new("FLQuant", ...)}.

}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{A five dimensional \code{array}  with dimensions named \code{quant},
    \code{year}, \code{unit}, \code{season} and \code{area}. Note that the \code{unit}
    dimension can be used to separate quantities by sex, by spawning status, ...}
    \item{\code{units}:}{An element of class \code{"character"} representing the units of
	magnitude of the data. Presently any character string is accepted, but users are
	encouraged to use standard acronyms, such as "t" for metric tonnes, "kg" for kilograms,
	and "1", "1e3", "1e6",... for individuals, thousands and millions of individuals 
	respectively.}
  }
}

\section{Extends}{
Class \code{"array"}.
}

\section{Generator}{
    The generator function is \code{\link{FLQuant}}. 
}

\section{Methods}{
    \describe{
Summarise contents:
    \item{\link{summary}:}{ Returns a list with a vector for each dimension
    in the object, which contains the levels of each dimension.}
    \item{\link{show}:}{ Prints the given object.}
    \item{\link{dims}:}{ Returns a list with containing number of ages,
    minimum age, maximum age, number of years, minimum year, maximum
    year, number of different units, number of seasons and number of
    areas in the object.}
    \item{\link{names}:}{ Returns a vector of strings containing the names
    of the various dimensions of the object.}
    \item{\link{units}:}{ Shows the contents of the \code{units} slot. See \code{\link{units}}}
    \item{\link{quant}:}{ Shows the name of the first dimension.}

Altering objects:
    \item{\link{units<-}:}{Modifies the contents of the \code{units} slot. See \code{\link{units}}}
	\item{\link{quant<-}}{Modifies the name of the first dimension. Any character string is allowed.}
	\item{\link{dimnames<-}}{Modifies the dimension names (dimnames) of an FLQuant object. Input must be a named list of the type name=value, where value is a character, or numeric, vector. Specifying new dimnames for less than five dimensions is allowed. If a name other than "year", "unit", "season" or "area" is used, this is taken to refer to the first dimension (quant). Only one name not on that list can be given.}

Indexing and subsetting:
    \item{\link{[}:}{To extract specific dimensions from the object.}
    \item{\link{[<-}:}{Values of selected dimensions of an object can be directly modified with this method.}
    \item{\link{window}:}{To specify a temporal window, along the year dimension,
		over which to subset or extend the object.}
	\item{\link{trim}:}{\code{signature(obj = "FLQuant")}}{
		This method provides a procedure to select a part of a FLQuant object using the dimnames of these objects.
		See \code{\link{trim}}
	}

Graphic output:
    \item{\link{plot}:}{Plots a bar chart}

Object coercion:
    \item{\link{as.data.frame}:}{Coerce an FLQuant into a data.frame, with
    columns for each dimension and an extra data column holding the actual
    data. Dimensions units, area and season are all coerced to factors, while
    year is coerced to numeric. The quant dimension is coerced to either of them
    depending on its content. Numeric values (like ages) will be converted to numeric
    while text labels will convert to factor. \code{units} is added as an attribute.}
    \item{tofrm:}{Returns a formula based on the FLQuant dimnames. There are to options by=("year","age"), the first returns something like \code{data~year|...}, the second \code{data~age|...}. This function is used for automatic plot development and should not be used for users ploting. This will be deprecated in the future when all users can use fully \code{xyplot} methods.}

   }
}

\author{FLR Team}
\seealso{
    \code{\link{FLQuant}}
    \code{\link{units}},
    \code{\link{quant}},
    \code{\link{is.FLQuant}},
    \code{\link{as.FLQuant}}
}
\examples{
# Generate an array containing random numbers, and convert it into an FLQuant
sq <- FLQuant(array(rnorm(120, mean=2), dim=c(20,3,2,1,1),
      dimnames=list(age=as.character(1:20), 
                    year=c("1999", "2000", "2001"), 
                    unit=c("male", "female"), 
                    season="all", area="all")), units="Tons")
summary(sq)
show(sq)
dims(sq)
names(sq)
sq <- sq[1:12,,1]
}
\keyword{classes}
\keyword{methods}
