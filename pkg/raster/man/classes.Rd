\name{RasterLayer-class}
\docType{class}

\alias{Raster-class}
\alias{RasterLayer-class}
\alias{RasterStack-class}
\alias{RasterBrick-class}

\alias{show,RasterLayer-method}
\alias{show,RasterStack-method}
\alias{show,RasterBrick-method}
\alias{hist,RasterLayer-method}
\alias{plot,Raster,missing-method}
\alias{plot,Raster,numeric-method}
\alias{dim,Raster-method}
\alias{summary,Raster-method}
\alias{[,RasterLayer-method}

\title{Classes for handling raster data }
\description{ \code{RasterLayer}, \code{RasterStack}, and \code{RasterBrick} (Raster* in short) are all classes for handling spatial raster (grid) data.

 RasterLayer represents a single layer raster.
 RasterStack represents a collection of RasterLayers with the same extent and resolution. 
 RasterBrick represents a single multi-layer raster.
 
 The difference between a RasterStack is that it typically points to several seperate data files on disk, whereas a RasterBrick points to a single file on disk. (But both can also not point to any file at all)
 
 Apart from with the normal new(" ") method, objects can be created with the following functions (among others)
 newRaster(), RasterFromFile(), newBrick(), BrickFromFile(), StackFromFile(), StackFromRasters(), StackFromFiles()
 
 RasterLayer and RasterBrick objects can also be created from SpatialPixels* and SpatialGrid* objects from the sp package: asRasterLayer(), asRasterBrick()
 Vice versa, Raster* objects can be coerced into a sp type oject with asSpGrid(). You can also use SetAs() 
 
 generic methods implemented for these classes are: 
 \code{summary}, \code{show}, \code{dim}, \code{hist}, \code{plot} (with a single object as argument, and in the case of hist and plot, with additional ... parameters)
 \code{plot} can also be supplied with an additional numeric argument indicating the layer in a RasterStack or RasterBrick object that should be plotted (mapped)
 \code{plot} can also be supplied with the two RasterLayer objects, creating a scatter plot (if they have matching extent and resolution)
 \code{[} is implemented for RasterLayer. It is equivalent to setValues() 
 
 These classes inherit from the \code{Raster}, which is a virtual class (objects cannot be created from it). However, many of the generic methods
 in this package are implemented for the Raster class so that they can be used with the other three classes.

 In case you really want a Raster object, rather than one of its decendants, to work with you can easily create your own class that is the same (but not virtual) with a command like this:
\code{setClass ('BasicRaster', contains = 'Raster')}
 Raster includes the \code{BoundingBox} class that is not virtual (e.g. see newBbox() )
 RasterLayer includes the classes \code{RasterFile} and \code{SingleLayerData}, whereas RasterStack and RasterBrick contain \code{RasterFile} and \code{MultipleLayerData}
 These classes are not further described here, because users should not need to directly access the slots in these classes. There are functions for that. Using these functions
 is much safer because a change in one slot should often affect the values in other slots. The 'setter' functions such as setValues take care of that. 

 }


\arguments{
  \item{x}{ AbsractRaster object }
  \item{raster}{ RasterLayer object }
  \item{...}{ additional arguments for the generic functions }
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
    \item{\code{bbox}:}{Object of class \code{"BoundingBox"} minimum and maximum x and y coordiantes }
    \item{\code{crs}:}{Object of class \code{"CRS"} }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Raster")}: displays information about the Raster* object }
    \item{summary}{\code{signature(object = "Raster")}: summary of Raster* values }
    \item{hist}{\code{signature(object = "RasterLayer")}: creates a histogram of the values of the RasterLayer object}
    \item{plot}{\code{signature(object = "Raster")}: creates a plot (map) of raster values }
    \item{plot}{\code{signature(object = "Raster", "numeric")}: creates a plot (map) of raster values. The second argument is the layer to be shown for multlayer objects }
    \item{dim}{\code{signature(object = "Raster")}: returns dimensions (ncol, nrow, and nlayers) of Raster* object}
 	 }
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\keyword{classes}
\keyword{spatial}
