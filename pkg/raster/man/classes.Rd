\name{RasterLayer-class}
\docType{class}


\alias{BoundingBox-class}
\alias{BasicRaster-class}
\alias{Raster-class}
\alias{RasterLayer-class}
\alias{RasterStack-class}
\alias{show,BoundingBox-method}
\alias{show,BasicRaster-method}
\alias{show,RasterLayer-method}
\alias{show,RasterStack-method}
\alias{hist,Raster-method}
\alias{plot,Raster,missing-method}
\alias{plot,Raster,numeric-method}
\alias{dim,BasicRaster-method}
\alias{summary,RasterLayer-method}
\alias{summary,RasterStack-method}
\alias{[,RasterLayer-method}
\alias{[<-,RasterLayer-method}
\alias{[,RasterStack-method}
\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[<-,RasterLayer,ANY,missing-method}

\title{Classes for handling raster data }
\description{ \code{RasterLayer}, \code{RasterStack} (Raster* in short) are all classes for handling spatial raster (grid) data.

 RasterLayer represents a single layer raster.
 RasterStack represents a collection of RasterLayers with the same extent and resolution; for example a multi-band remote sensing image. 
 
 The difference between a RasterStack is that it typically points to several seperate data files on disk, but it could also point to different bands from a single file (or not not point to any file at all)
 
 Apart from with the normal new(" ") method, objects can be created with the following functions (among others)
 newRaster(), RasterFromFile(), StackFromFile(), StackFromRasters(), StackFromFiles()
 
 RasterLayer and RasterStack objects can also be created from SpatialPixels* and SpatialGrid* objects from the sp package: asRasterLayer(), asRasterStack()
 Vice versa, Raster* objects can be coerced into a sp type oject with asSpGrid(). You can also use SetAs() 
 
 generic methods implemented for these classes are: 
 \code{summary}, \code{show}, \code{dim}, \code{hist}, \code{plot} (with a single object as argument, and in the case of hist and plot, with additional ... parameters)
 \code{plot} can also be supplied with an additional numeric argument indicating the layer in a RasterStack object that should be plotted (mapped)
 \code{plot} can also be supplied with the two RasterLayer objects, creating a scatter plot (if they have matching extent and resolution)
 \code{[} is implemented for RasterLayer. It is equivalent to setValues() 
 
 These classes inherit from the \code{Raster}, which is a virtual class (objects cannot be created from it). However, many of the generic methods
 in this package are implemented for the Raster class so that they can be used with the other three classes.

 In case you really want a Raster object, rather than one of its decendants, to work with you can easily create your own class that is the same (but not virtual) with a command like this:
\code{setClass ('BasicRaster', contains = 'Raster')}
 Raster includes the \code{BoundingBox} class that is not virtual (e.g. see newBbox() )
 RasterLayer includes the classes \code{RasterFile} and \code{SingleLayerData}, whereas RasterStack contains \code{RasterFile} and \code{MultipleLayerData}
 These classes are not further described here, because users should not need to directly access the slots in these classes. There are functions for that. Using these functions
 is much safer because a change in one slot should often affect the values in other slots. The 'setter' functions such as setValues take care of that. 

 }


\arguments{
  \item{x}{ AbstractRaster object }
  \item{raster}{ RasterLayer object }
  \item{...}{ additional arguments for the generic functions }
}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer")}, but you should normally use functions such as newRaster() and rasterFromFile().
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
    \item{plot}{\code{signature(object = "Raster")}: creates a plot (map) of raster values }
    \item{plot}{\code{signature(object = "Raster", "numeric")}: creates a plot (map) of raster values. The second argument is the layer to be shown for multlayer objects }
    \item{dim}{\code{signature(object = "Raster")}: returns dimensions (ncol, nrow, and nlayers) of Raster* object}
 	 }
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\keyword{classes}
\keyword{spatial}
