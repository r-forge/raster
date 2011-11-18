\name{vectorAlgebra}


\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}

\title{Algebra (notation) with polygons}

\description{
Under development!

Given SpatialPolygons* object \code{x} and \code{y}
\code{x + y}  --  same as \code{\link{merge}}
\code{x - y}  --  same as \code{\link[rgeos]{gDifference}}
\code{x * y}  --  same as \code{\link{crop}}
}

\seealso{ \code{\link[raster]{merge}, \link[raster]{aggregate}}, \link[raster]{crop} }

\author{Robert J. Hijmans}

\examples{
r1 <- r2 <- raster(nrow=10, ncol=10)
r1[] <- (runif(ncell(r1))-0.5) * 10
r2[] <- (runif(ncell(r1))-0.5) * 10
atan2(r1, r2)
}

\keyword{methods}
\keyword{math}
