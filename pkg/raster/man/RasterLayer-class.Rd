\name{Raster-class}

\docType{class}

\alias{BasicRaster-class}
\alias{Raster-class}
\alias{RasterLayer-class}
\alias{RasterStack-class}

\alias{show,BasicRaster-method}
\alias{show,RasterLayer-method}

\title{ Raster* classes}


\description{ \code{RasterLayer} and \code{RasterStack} (Raster* in short) are classes for handling spatial raster (grid) data.

 RasterLayer represents a single layer raster.  RasterStack represents a collection of RasterLayers with the same extent and resolution; for example a multi-band remote sensing image. 
 
A RasterStack is that it typically points to several seperate data files on disk, but it could also point to different bands from a single file (or not not point to any file at all)
 
Apart from with the normal new(" ") method, objects can be created with the following functions (among others)
\code{raster(), RasterFromFile(), stack(),  StackFromFile(), StackFromRasters(), StackFromFiles()}
 
 RasterLayer and RasterStack objects can also be created from SpatialPixels* and SpatialGrid* objects from the sp package using \code{as}
 or the explicit functions \code{asRasterLayer()} and \code{asRasterStack()}
 Vice versa, Raster* objects can be coerced into a sp type oject with \code{asSpGrid()}. 
 
 generic methods implemented for these classes include: 
 \code{summary}, \code{show}, \code{dim}, \code{hist},
 \code{plot} (with a single object as argument, and in the case of hist and plot, with additional ... parameters)
 \code{[} is implemented for RasterLayer. It is equivalent to setValues() 
 
 These classes inherit from the \code{Raster}, which is a virtual class (objects cannot be created from it). However, many of the generic methods
 in this package are implemented for the Raster class so that they can be used with the other three classes.

 In case you really want a Raster object, rather than one of its decendants, to work with you can easily create your own class that is the same (but not virtual) with a command like this:
\code{setClass ('BasicRaster', contains = 'Raster')}
 Raster includes a slot with an object of the \code{BoundingBox} class )
 RasterLayer includes the classes \code{RasterFile} and \code{SingleLayerData}, whereas RasterStack contains \code{RasterFile} and \code{MultipleLayerData}
 These classes are not further described here because users should not need to directly access these slots. 
 The 'setter' functions such as setValues should be used instead. Using such 'set' functions
 is much safe because a change in one slot should often affect the values in other slots. 
 }


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer", ...)}, or with the helper functions such as \code{raster}.
}
\section{Slots}{
Slots for RasterLayer and RasterStack objects

	\describe{
    \item{\code{title}:}{Object of class \code{"character"} }
    \item{\code{file}:}{Object of class \code{"RasterFile"} }
    \item{\code{data}:}{Object of class \code{"SingleLayerData"} or \code{"MulitpleLayerData"}}
    \item{\code{history}:}{Object of class \code{"vector"} }
    \item{\code{bbox}:}{Object of class \code{"BoundingBox"} }
    \item{\code{ncols}:}{Object of class \code{"integer"} }
    \item{\code{nrows}:}{Object of class \code{"integer"} }
    \item{\code{crs}:}{Object of class \code{"CRS"} }
  }
}
\section{Extends}{
Classes \code{RasterLayer} and \code{RasterStack} extend class \code{"\linkS4class{Raster}"}, directly.
Classes \code{RasterLayer} and \code{RasterStack} extend class \code{"\linkS4class{BasicRaster}"}, by class "Raster", distance 2.
}
\section{Methods}{
  \describe{
    \item{!}{\code{signature(x = "RasterLayer")}: ... }
    \item{[}{\code{signature(x = "RasterLayer")}: ... }
    \item{[[}{\code{signature(x = "RasterLayer", i = "ANY", j = "ANY")}: ... }
    \item{[[<-}{\code{signature(x = "RasterLayer", i = "ANY", j = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "RasterLayer")}: ... }
    \item{aggregate}{\code{signature(x = "RasterLayer")}: ... }
    \item{calc}{\code{signature(x = "RasterLayer", fun = "function")}: ... }
    \item{cellValues}{\code{signature(object = "RasterLayer", cells = "vector")}: ... }
    \item{is.finite}{\code{signature(x = "RasterLayer")}: ... }
    \item{is.infinite}{\code{signature(x = "RasterLayer")}: ... }
    \item{is.na}{\code{signature(x = "RasterLayer")}: ... }
    \item{is.nan}{\code{signature(x = "RasterLayer")}: ... }
    \item{merge}{\code{signature(x = "RasterLayer", y = "RasterLayer")}: ... }
    \item{overlay}{\code{signature(x = "RasterLayer", y = "RasterLayer")}: ... }
    \item{plot}{\code{signature(x = "RasterLayer", y = "RasterLayer")}: ... }
    \item{readAll}{\code{signature(object = "RasterLayer")}: ... }
    \item{readBlock}{\code{signature(object = "RasterLayer")}: ... }
    \item{readPartOfRow}{\code{signature(object = "RasterLayer")}: ... }
    \item{readRow}{\code{signature(object = "RasterLayer")}: ... }
    \item{readRows}{\code{signature(object = "RasterLayer")}: ... }
    \item{setValues}{\code{signature(object = "RasterLayer")}: ... }
    \item{show}{\code{signature(object = "RasterLayer")}: ... }
    \item{stack}{\code{signature(x = "RasterLayer")}: ... }
    \item{summary}{\code{signature(object = "RasterLayer")}: ... }
    \item{xyValues}{\code{signature(object = "RasterLayer", xyCoords = "matrix")}: ... }
	
	
    \item{asRasterLayer}{\code{signature(object = "RasterStack")}: ... }
    \item{calc}{\code{signature(x = "RasterStack", fun = "function")}: ... }
    \item{calc}{\code{signature(x = "RasterStack", fun = "list")}: ... }
    \item{cellValues}{\code{signature(object = "RasterStack", cells = "vector")}: ... }
    \item{nlayers}{\code{signature(object = "RasterStack")}: ... }
    \item{overlay}{\code{signature(x = "RasterStack", y = "missing")}: ... }
    \item{readAll}{\code{signature(object = "RasterStack")}: ... }
    \item{readPartOfRow}{\code{signature(object = "RasterStack")}: ... }
    \item{readRow}{\code{signature(object = "RasterStack")}: ... }
    \item{setValues}{\code{signature(object = "RasterStack")}: ... }
    \item{show}{\code{signature(object = "RasterStack")}: ... }
    \item{summary}{\code{signature(object = "RasterStack")}: ... }
    \item{xyValues}{\code{signature(object = "RasterStack", xyCoords = "matrix")}: ... }
    \item{xyValues}{\code{signature(object = "RasterStack", xyCoords = "SpatialPoints")}: ... }
	
	 }
}

\author{ Robert J. Hijmans }


\examples{
showClass("RasterLayer")
}

\keyword{classes}
\keyword{spatial}
