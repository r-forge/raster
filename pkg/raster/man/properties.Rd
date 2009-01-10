\name{properties}
\alias{filename}
\alias{ncol}
\alias{nrow}
\alias{nrow,AbstractRaster-method}
\alias{ncol,AbstractRaster-method}
\alias{ncells}
\alias{xres}
\alias{yres}
\alias{resolution}
\alias{xmin}
\alias{xmax}
\alias{ymin}
\alias{ymax}
\alias{projection}
\alias{origin}
\alias{nlayers}
\alias{layers}
\alias{band}
\alias{nbands}
 
  
\title{ RasterLayer properties }
\description{
  get the column, row, or cell number of a Raster (or RasterStack) for a x and/or y coordinate or get the coordinates of the center of a raster-cell from a column, or cell number(s)
  
}

\usage{
filename(object)
ncol(x)
nrow(x)
ncells(object)
xres(object)
yres(object)
resolution(object)
xmin(object)
xmax(object)
ymin(object)
ymax(object)
projection(object, asText=TRUE)
origin(object)
nlayers(object)
layers(object)
band(object)
nbands(object)
}

\arguments{
  \item{object}{ Raster* object }
  \item{x}{ Raster* object }
  \item{asText} { logical. If \code{TRUE}, the projection is returned as text. Otherwise a CRS object is returned}
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
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
rs <- newRaster()
ncells(rs)
ncol(rs)
nrow(rs)
ncells(rs)
xres(rs)
yres(rs)
resolution(rs)
xmin(rs)
xmax(rs)
ymin(rs)
ymax(rs)
projection(rs)
origin(rs)
nbands(rs)
band(rs)
}
\keyword{ spatial }
