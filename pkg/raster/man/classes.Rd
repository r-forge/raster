\name{classes}
\docType{class}

\alias{show,RasterStack-method}
\alias{hist,Raster-method}
\alias{dim,BasicRaster-method}
\alias{summary,RasterLayer-method}
\alias{show,RasterLayerSummary-method}
\alias{summary,RasterStack-method}

\title{Replace methods }
\description{ 
   to be moved ....
 }


\arguments{
  \item{x}{ AbstractRaster object }
  \item{raster}{ RasterLayer object }
  \item{...}{ additional arguments for the generic functions }
}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer")}, but you should normally use functions such as raster() and rasterFromFile().
}

\section{Slots}{
	\describe{
	\item{\code{title}:}{Object of class \code{"character"}. Short name for raster } 
	\item{\code{file}:}{ Object of class \code{"RasterFile"}} 
	\item{\code{data}:}{ Object of class \code{"RasterData"}} 
	\item{\code{history}:}{Object of class \code{"character"}} 
    \item{\code{ncols}:}{Object of class \code{"integer"} number of columns }
    \item{\code{nrows}:}{Object of class \code{"integer"} number of rows }
    \item{\code{bbox}:}{Object of class \code{"BoundingBox"} minimum and maximum x and y coordinates }
    \item{\code{crs}:}{Object of class \code{"CRS"} }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Raster")}: displays information about the Raster* object }
    \item{summary}{\code{signature(object = "Raster")}: summary of Raster* values }
    \item{hist}{\code{signature(object = "RasterLayer")}: creates a histogram of the values of the RasterLayer object}
    \item{dim}{\code{signature(object = "Raster")}: returns dimensions (ncol, nrow, and nlayers) of Raster* object}
 	 }
}
\author{Robert J. Hijmans}

\keyword{classes}
\keyword{spatial}
