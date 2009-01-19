\name{Coercion}
\alias{asRasterLayer,missing,missing-method}
\alias{asRasterLayer,character,missing-method}
\alias{asRasterLayer,Raster,missing-method}
\alias{asRasterLayer,RasterLayer,numeric-method}
\alias{asRasterLayer,RasterStackBrick,numeric-method}
\alias{asRasterLayer,SpatialPixels,missing-method}
\alias{asRasterLayer,SpatialPixels,numeric-method}
\alias{asRasterLayer,SpatialPixelsDataFrame,numeric-method}
\alias{asRasterLayer,SpatialGridDataFrame,numeric-method}

\alias{asRasterLayer}
\alias{asRasterBrick}
\alias{asSpGrid}

\title{ Coercion of Raster*, SpatialGrid and SpatialPixels* objects. }

\description{  
	Functions to coerce a Raster* objects into an sp SpatialGridDataFrame object and 
	to coerce a RasterLayer or RasterBrick object from a sp SpatialPixels* or SpatialGrid* object
}

\usage{
asRasterLayer(object, index) 
asRasterBrick(spgrid) 
asSpGrid(object, type='grid') 
}

\arguments{
  \item{object}{ a Raster* type object (SpatialPixel, SpatialPixelDataFrame , SpatialGrid, or SpatialGridDataFrame }
  \item{spgrid}{ a sp raster type object (SpatialPixel, SpatialPixelDataFrame , SpatialGrid, or SpatialGridDataFrame }
  \item{index}{integer values >= 1 indicating the layer in the RasterStack or RasterBrick, or the column in the sp object dataframe to take the values from}
  \item{type}{character. Indicating what type of sp object to return. Should be 'pixel' or 'grid'.}
}

\details{
 if \code{type} is 'pixel' a SpatialPixel* object is returned. If the RasterLayer object has data a SpatialPixelDataFrame will be returned.
 if \code{type} is 'grid' a SpatialGrid* object is returned. If the RasterLayer object has data a SpatialGridDataFrame will be returned.
}

\value{
	a Raster or a sp object
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{ 
	rs <- newRaster()
	rs <- setValues(rs, 1:ncell(rs))
	sp <- asSpGrid(rs)
	rs2 <- asRasterLayer(sp) 
}

\keyword{ spatial }
