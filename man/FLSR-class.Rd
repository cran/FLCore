% $ Id: $
\name{FLSR-class}
\docType{class}
\alias{FLSR-class}
\alias{plot,FLSR,missing-method}
\alias{predict,FLSR-method}
\alias{summary,FLSR-method}
\alias{params,FLSR-method}
\alias{params<-,FLSR-method}
\alias{params<-,FLSR,vector-method}
\alias{params<-,FLSR,matrix-method}
\alias{name,FLSR-method}
\alias{name<-,FLSR,character-method}
\alias{desc,FLSR-method} 
\alias{desc<-,FLSR,character-method}
\alias{model,FLSR-method}
\alias{model<-,FLSR,character-method}
\alias{ssb,FLSR-method}
\alias{ssb<-,FLSR,FLQuant-method}
\alias{rec,FLSR-method}
\alias{rec<-,FLSR,FLQuant-method}
\alias{rechat,FLSR-method}
\alias{rechat<-,FLSR,FLQuant-method}
\alias{residuals,FLSR-method}
\alias{residuals<-,FLSR,FLQuant-method}
\alias{params,FLSR-method}
\alias{params<-,FLSR,matrix-method}
\alias{se,FLSR-method}
\alias{se<-,FLSR,numeric-method}
\alias{covar,FLSR-method}
\alias{covar<-,FLSR,matrix-method}
\alias{var,FLSR,missing,missing,missing-method}
\alias{var<-}
\alias{"var<-"-methods}
\alias{var<-,FLSR,numeric-method}
\alias{varacorr,FLSR-method}
\alias{varacorr<-,FLSR,numeric-method}
\alias{aic,FLSR-method}
\alias{aic<-,FLSR,numeric-method}
\alias{mcmc,FLSR-method}
\alias{mcmc<-,FLSR,list-method}
\alias{qapply,FLSR,function-method}

\title{Class FLSR}
\description{A class for stock-recruitment data and models}
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("FLSR", ...)} or through
    the constructor function \code{FLSR()}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{ the name of the stock.}
    \item{\code{desc}:}{ a description of the data.}
    \item{\code{model}:}{ name of the stock-recruitment relationship used in the model.}
    \item{\code{ssb}:}{ a FLQuant object with Spawning Stock Biomass data.}
    \item{\code{rec}:}{ a FLQuant object with recruitment data.}
    \item{\code{rechat}:}{ a FLQuant object with estimated recruitment.}
    \item{\code{residuals}:}{ a FLQuant object with recruitment residuals.}
    \item{\code{params}:}{ a matrix containing the parameters of the SR relationship.}
    \item{\code{se}:}{ a numeric vector containing the standard error of the SR parameters.}
    \item{\code{covar}:}{ the covariance matrix of the SR parameters.}
    \item{\code{var}:}{ the variance of log-residuals.}
    \item{\code{varacorr}:}{ the residual variance after auto-correlation has been taken out.}
    \item{\code{aic}:}{ the Akaike Information Criterion index.}
    \item{\code{mcmc}:}{ a list with all the MCMC information required for the Bayesian runs.} 
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(obj = "FLSR")}: Plots different graphs related to the fit of the SR model to SR data.}
    \item{predict}{\code{signature(obj = "FLStock", ssb = c('numeric', "FLQuant"))}: Predicts recruitment values from the model parameters
	   estimates contained in an FLSR object, and an SSB value. If \code{ssb} is of class \code{FLQuant},
	   \code{predict} returns another \code{FLQuant}. If a numeric vector is input, a vector of the same
	   length is returned.}
    \item{summary}{\code{signature(obj = "FLSR")}:Prints a detailed summary of the
    slots in the object, indicating whether they contain any data and the number of years,
    quants, areas, units and seasons specified.}
    \item{qapply}{\code{signature(X = "FLSR", FUN = "function")}: Apply a function to all
	FLQuant slots in an object}
    }
}
\author{FLR Team}
\seealso{
  \code{\link{sr}} function fits different stock-recruitment models to the data stored in the 'FLSR' objects.
  \code{\link{as.FLSR}} creates an 'FLSR' object from a 'FLStock' or 'FLBiol' object.
  \code{\link{FLStock-class}}, \code{\link{FLBiol-class}}.
}
\examples{
    data(ple4)

    fsr <- as.FLSR(ple4, model = "ricker")
    fsr <- sr(fsr)
    plot(fsr)
    params(fsr)

}
\keyword{classes}
\keyword{methods}
