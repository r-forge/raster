\name{vectorAlgebra}

\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}
\alias{^,SpatialPolygons,SpatialPolygons-method}

\title{Algebra (notation) for overlaying SpatialPolygons* }

\description{
These funcitons are experimental. Under development!

Given SpatialPolygons* object \code{x} and \code{y}

\code{x + y}  --  same as \code{\link{merge}}

\code{x * y}  --  same as \code{\link{crop}}

\code{x - y}  

Using functions from rgeos, with some added functionality to keep attributes.

See \link[raster]{Arith-methods} for similar arithmetic with Raster objects
}


\details{

Comparison of terminology for polygon overlay methods used in ArcGIS and raster 
\tabular{rll}{
  \tab ArcGIS    \tab  raster  \cr
  \tab dissolve  \tab  aggregate  \cr
  \tab append    \tab  "c" \cr
  \tab union     \tab  merge or "+" \cr
  \tab intersect \tab  crop or "*" \cr
  \tab erase     \tab  "-" \cr
  \tab symdif    \tab  "^" (to be done) \cr
}
 
To do: identity /update 

For spatialjoin and other spatial queries, see sp functions over and overlay
 
}


\seealso{ \code{\link[raster]{merge}, \link[raster]{aggregate}}, \link[raster]{crop} }

\author{Robert J. Hijmans}

\keyword{methods}
\keyword{spatial}
