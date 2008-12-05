\name{RasterLayer-class}
\docType{class}
\alias{RasterLayer-class}
\alias{show,RasterLayer-method}
\alias{hist,RasterLayer-method}
\alias{hist}
\alias{dim,AbstractRaster-method}
\alias{dim,RasterStack-method}
\alias{dim,RasterBrick-method}



\title{Class "RasterLayer" }
\description{	Class for handling RasterLayer (grid) data  }

\usage{
  hist(x, ...)
}

\arguments{
  \item{x}{ RasterLayer object }
  \item{...}{ additional arguments for the generic hist function }
}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer", ...)}.
}

\section{Slots}{
	\describe{
	\item{\code{title}:}{Object of class \code{"character"}. Short name for raster } 
	\item{\code{file}:}{ Object of class \code{"RasterFile"}} 
	\item{\code{data}:}{ Object of class \code{"RasterData"}} 
	\item{\code{history}:}{Object of class \code{"character"}} 
    \item{\code{ncols}:}{Object of class \code{"integer"} number of columns }
    \item{\code{nrows}:}{Object of class \code{"integer"} number of rows }
    \item{\code{bbox}:}{Object of class \code{"maxtrix"} min and max x and y coordiantes }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RasterLayer")}: displays information about the RasterLayer object }
    \item{hist}{\code{signature(object = "RasterLayer")}: creates a histogram of the values of the RasterLayer }
    \item{dim}{\code{signature(object = "AbstractRaster")}: returns dimensions (ncol and nrow) }
    \item{dim}{\code{signature(object = "RasterStack")}: returns dimensions (ncol, nrow, and nlayers) }
    \item{dim}{\code{signature(object = "RasterBrick")}: returns dimensions (ncol, nrow, and nlayers) }
 	 }
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\keyword{classes}
\keyword{spatial}
