\name{properties}
\alias{filename}
\alias{filename<-}
\alias{ncol}
\alias{nrow}
\alias{nrow,BasicRaster-method}
\alias{ncol,BasicRaster-method}
\alias{ncell}
\alias{xres}
\alias{yres}
\alias{resolution}
\alias{xmin}
\alias{xmax}
\alias{ymin}
\alias{ymax}
\alias{projection}
\alias{isLatLon}
\alias{isLatLon,Raster-method}
\alias{isLatLon,CRS-method}
\alias{isLatLon,character-method}
\alias{projection<-}
\alias{origin}
\alias{nlayers}
\alias{nlayers,BasicRaster-method}
\alias{nlayers,Raster-method}
\alias{nlayers,RasterStack-method}
\alias{nlayers,Spatial-method}
\alias{layerNames}
\alias{band}
\alias{nbands}
\alias{dataType} 
\alias{dataSize} 

  
\title{RasterLayer properties}
\description{
  get the column, row, or cell number of a Raster (or RasterStack) for a x and/or y coordinate or get the coordinates of the center of a raster-cell from a column, or cell number(s)
  
}

\usage{
filename(object)
ncol(x)
nrow(x)
ncell(object)
ncell(object)
xres(object)
yres(object)
resolution(object)
xmin(object)
xmax(object)
ymin(object)
ymax(object)
projection(object, asText=TRUE)
isLatLon(object)
origin(object)
dataSize(object)
dataType(object)
nlayers(object)
layerNames(object)
band(object)
nbands(object)
}

\arguments{
  \item{object}{Raster* object}
  \item{x}{Raster* object}
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a CRS object is returned}
}

\note{
  Some of these functions simply take the values from the data slots in the RasterLayer or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\value{
  row, col or cell number(s) 
  x or y coordinate(s)
  matrix(x,y) of pairs of coordinates 
  matrix(cell, x, y) of cellnumbers and x and y coordinates
}
\author{Robert J. Hijmans }
\examples{
#using a new default raster (1 degree global)
r <- raster()
ncell(r)
ncol(r)
nrow(r)
ncell(r)
xres(r)
yres(r)
resolution(r)
xmin(r)
xmax(r)
ymin(r)
ymax(r)
projection(r)
origin(r)
nbands(r)
band(r)
}
\keyword{spatial}
