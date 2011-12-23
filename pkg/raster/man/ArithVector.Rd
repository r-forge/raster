\name{vector algebra}

\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{+,SpatialPolygonsDataFrame,data.frame-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}
\alias{^,SpatialPolygons,SpatialPolygons-method}

\title{Algebra (notation) for manipulating SpatialPolygons* }

\description{
These funcitons are experimental. Under development!

Given SpatialPolygons* object \code{x} and \code{y}

\code{x+y} This is the same as \code{\link{merge}(x, y)}

\code{x*y} This is the same as \code{\link{crop}(x, y)}

\code{x-y} Erase the area covered by \code{y} from \code{x}

These functions are based on functions from rgeos, with some added functionality to, among other things, avoid polygon duplication and to keep the attributes.

See \link[raster]{Arith-methods} for similar arithmetic with Raster objects
}


\details{

Comparison of terminology for polygon overlay methods used in ArcGIS and in this package (raster)

\tabular{rll}{
  \tab \bold{ArcGIS}    \tab  \bold{raster} \cr
  \tab Erase     \tab  "-" \cr
  \tab Identity  \tab  update(x, y, ..., instersect=TRUE) \cr
  \tab Intersect (join attributes) \tab  intersect \cr
  \tab Spatial Join \tab  "^" (to be done) \cr  
  \tab Symmetrical Difference \tab  "^" (to be done) \cr
  \tab Union     \tab  merge or "+" \cr
  \tab Update    \tab  update \cr
}


For spatial-join and other spatial queries, see sp functions 'over' and 'overlay' (or raster function extract when using Raster objects)

Other functions for manipulating vector type data

\tabular{rll}{
  \tab \bold{ArcGIS}    \tab  \bold{raster} \cr
  \tab append    \tab  merge(x, y, ..., intersect=FALSE) \cr
  \tab dissolve  \tab  aggregate  \cr
  \tab explode   \tab  disaggregate  \cr
  \tab clip      \tab  crop or "*" \cr
  \tab select    \tab  select \cr
  \tab "i"       \tab  click \cr
}
}


\seealso{\code{ \link[raster]{aggregate}, \link{crop}, \link{merge,SpatialPolygons,SpatialPolygons-method}} }

\author{Robert J. Hijmans}

\keyword{methods}
\keyword{spatial}
