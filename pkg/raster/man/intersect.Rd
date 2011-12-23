\name{intersect}

\docType{methods}

\alias{intersect,SpatialPolygons,SpatialPolygons-method}
\alias{intersect,Extent,Extent-method}

\title{ 
Intersect SpatialPolygons
}

\description{
Intersect two SpatialPolygons* objects or two Extent objects. Only the overlapping areas (if any) are returnred. For SpatialPolygonDataFrame objects, the data.frames are also merged. 
}

\usage{
\S4method{intersect}{SpatialPolygons,SpatialPolygons}(x, y)

\S4method{intersect}{Extent,Extent}(x, y)
}

\arguments{
  \item{x}{SpatialPolygons* or Extent object}
  \item{y}{SpatialPolygons* or Extent object}
} 

\note{
use \code{\link{crop}} if you want to intersect a Spatial* object with an Extent object.
}


\value{
SpatialPolygons* or Extent
}

\author{Robert J. Hijmans}

\seealso{
The \code{\link{merge,SpatialPolygons,SpatialPolygons-method}} for appending and perhaps spatially intersecting polygons

The \code{\link{merge,Raster,Raster-method}} for merging Raster objects
}


\keyword{methods}
\keyword{spatial}

