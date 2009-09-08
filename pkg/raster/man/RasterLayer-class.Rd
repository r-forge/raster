\name{Raster-class}

\docType{class}

\alias{BasicRaster-class}
\alias{Raster-class}
\alias{RasterLayer-class}
\alias{RasterStack-class}

\alias{show,BasicRaster-method}
\alias{show,RasterLayer-method}
\alias{show,RasterStack-method}


\title{ Raster* classes}


\description{ \code{RasterLayer} and \code{RasterStack} (Raster* in short) are classes for handling spatial raster (grid) data.

RasterLayer represents a single layer raster.  RasterStack represents a collection of RasterLayers with the same extent and resolution;
for example a multi-band remote sensing image. 
 
Objects can be created with the following helper functions: \code{\link[raster]{raster} and \link[raster]{stack}}  
 
RasterLayer and RasterStack objects can also be created from SpatialPixels* and SpatialGrid* objects from the sp package using \code{as}
or the explicit function \code{\link[raster]{asRasterLayer}}. 
Vice versa, Raster* objects can be coerced into a sp type oject with \code{as( , )} and with \code{asSpGrid()}. 
 
Common generic methods implemented for these classes include: 
 
 \code{summary}, \code{show}, \code{dim}, \code{hist},
 
 \code{plot} (with a single object as argument, and in the case of hist and plot, with additional ... parameters)
 
 \code{[} is implemented for RasterLayer. 
 
 These classes inherit from the \code{Raster}, which is a virtual class (objects cannot be created from it). However, many of the generic methods
 in this package are implemented for the Raster class so that they can be used with the other three classes.

 Raster includes a slot with an object of the \code{BoundingBox} class )

 RasterLayer includes the classes \code{RasterFile} and \code{SingleLayerData}, 
 whereas RasterStack contains \code{RasterFile} and \code{MultipleLayerData}
 
 These classes are not further described here because users should not need to directly access these slots. 
 The 'setter' functions such as \code{setValues} should be used instead. Using such 'setter' functions
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
    \item{\code{crs}:}{Object of class \code{"CRS"}, i.e. the coordinate reference system }
  }
}
\section{Extends}{
Classes \code{RasterLayer} and \code{RasterStack} extend class \code{"\linkS4class{Raster}"}, directly.
Classes \code{RasterLayer} and \code{RasterStack} extend class \code{"\linkS4class{BasicRaster}"}, by class "Raster", distance 2.
}

\section{Methods}{
  \describe{
  
    \item{[}{\code{signature(x = "RasterLayer", i = "ANY", j = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "RasterLayer", i = "ANY", j = "ANY")}: ... }
    \item{plot}{\code{signature(x = "RasterLayer", y = "RasterLayer")}: ... }
    \item{show}{\code{signature(object = "RasterLayer")}: ... }
    \item{summary}{\code{signature(object = "RasterLayer")}: ... }
	
    \item{raster}{\code{signature(object = "RasterStack")}: ... }
    \item{show}{\code{signature(object = "RasterStack")}: ... }
    \item{summary}{\code{signature(object = "RasterStack")}: ... }
	
	And many other methods (see the other entries in this help file)
	}
}

\author{ Robert J. Hijmans }


\examples{
showClass("RasterLayer")
}

\keyword{classes}
\keyword{spatial}
