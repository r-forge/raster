\name{Coercion}
\alias{asRasterLayer,RasterStack-method}
\alias{asRasterLayer,SpatialPixelsDataFrame-method}
\alias{asRasterLayer,SpatialGridDataFrame-method}
\alias{asRasterLayer}

\title{ Coercion of SpatialGridDataFrame and SpatialPixelsDataFrame objects to a RasterLayer. }

\description{  
Functions to coerce a SpatialGridDataFrame, SpatialPixelsDataFrame, and RasterStack objects to a RasterLayer object. 
You can use as( , ) for these coercions (see examples), and for many other coercions between sp and raster objects, but asRasterLayer allows for indicating which variable should be passed to the RasterLayer object.
RasterLayer objects only have a single variable, whereas a RasterStack and the Spatial* objects can have multiple variables.
}

\usage{
asRasterLayer(object, index=1) 
}

\arguments{
  \item{object}{ a Raster* type object (SpatialPixel, SpatialPixelDataFrame , SpatialGrid, or SpatialGridDataFrame }
  \item{index}{integer >= 1 indicating the layer in the RasterStack, or the column in the sp object dataframe to take the values from}
}

\details{
 if \code{type} is 'pixel' a SpatialPixel* object is returned. If the RasterLayer object has data a SpatialPixelDataFrame will be returned.
 if \code{type} is 'grid' a SpatialGrid* object is returned. If the RasterLayer object has data a SpatialGridDataFrame will be returned.
 
 In most cases you can also coerce objects using setAs, except that you cannot change the default 'index' (variable) and the first variable (column of the data frame of a Spatial* object) is used.
 
 e.g.: as(aSpatialPixelsDataFrame, "RasterStack") or as(aRasterLayer, "SpatialGridDataFrame")
}

\value{
a RasterLayer object
}
\author{ Robert J. Hijmans}

\seealso{ \code{\link[raster]{RasterLayer-class}}, \code{\link[sp]{SpatialGridDataFrame-class}}, \code{\link[methods]{as}}}

\examples{ 
r1 <- raster(ncols=90, nrows=45)
r1 <- setValues(r1, 1:ncell(r1))
r2 <- setValues(r1, 1:ncell(r1))
stk <- makeStack(r1, r2)
sp <- as(stk, 'SpatialGridDataFrame')
r3 <- asRasterLayer(sp, 2) 
as(r2, 'BasicRaster') == as(r3, 'BasicRaster')
sum(values(r2) == values(r3)) == ncell(r2)
r4 <- asRasterLayer(stk, 1)
sp <- as(r4, 'SpatialPixels')
sp <- as(r4, 'SpatialGridDataFrame')
}

\keyword{ spatial }
