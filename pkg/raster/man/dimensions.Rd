\name{dimensions}

\alias{ncol}
\alias{nrow}
\alias{nrow,BasicRaster-method}
\alias{ncol,BasicRaster-method}
\alias{dim}
\alias{dim,BasicRaster-method}
\alias{ncell}
\alias{ncell,ANY-method}
\alias{length,Raster-method}
\alias{xres}
\alias{yres}
\alias{resolution}
\alias{nlayers}
\alias{nlayers,BasicRaster-method}
\alias{nlayers,Raster-method}
\alias{nlayers,RasterStack-method}
\alias{nlayers,Spatial-method}
\alias{layerNames}
\alias{band}
\alias{nbands}
  
\title{Dimensions}

\description{
  Get dimension properties of a Raster* object
}

\usage{
ncol(x)
nrow(x)
dim(x)
ncell(x)
xres(object)
yres(object)
resolution(object)
nlayers(object)
layerNames(object)
band(object)
nbands(object)
}

\arguments{
  \item{object}{Raster* object}
  \item{x}{Raster* object}
}

\note{
  Some of these functions simply take the values from the data slots in the Raster* object.
  It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. 
  This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\value{
  in most cases a single numeric
  
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
origin(r)
nbands(r)
band(r)
}

\keyword{spatial}
