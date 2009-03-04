\name{extent}

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

  
\title{Raster extent}
\description{
  Extent and projection of Raster* objects
}

\usage{
xmin(object)
xmax(object)
ymin(object)
ymax(object)
projection(object, asText=TRUE)
isLatLon(object)
}

\arguments{
  \item{object}{Raster* object}
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a CRS object is returned}
}

\note{
  Some of these functions simply take the values from the data slots in the RasterLayer or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\value{
  varies
}

\author{Robert J. Hijmans }

\examples{
r <- raster()
xmin(r)
xmax(r)
ymin(r)
ymax(r)
projection(r)
}

\keyword{spatial}
