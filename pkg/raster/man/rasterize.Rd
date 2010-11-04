\name{rasterize}

\docType{methods}

\alias{rasterize}
\alias{rasterize,matrix,Raster-method}
\alias{rasterize,SpatialPoints,Raster-method}
\alias{rasterize,SpatialLines,Raster-method}
\alias{rasterize,SpatialPolygons,Raster-method}
\alias{rasterize,Extent,Raster-method}

\title{Rasterize points, lines, or polygons}

\description{
Rasterize d

This function has replaced four other functions in this package (pointsToRaster, linesToRaster, polygonsToRaster)
}

\usage{
rasterize(x, y, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{points (a SpatialPoints* object or a two-column matrix), SpatialLines*, or SpatialPolygons*, or an Extent object }
  \item{...}{Additional arguments, see under Details}  
}

\details{
\bold{= points =}

If \code{y} represents points, \code{rasterize} returns 

\bold{= polygons =}

If \code{y} represents polygons, the \code{rasterize} method 

\bold{= lines =}

If \code{y} represents lines, 
}


\value{
RasterLayer
}


\seealso{ \code{\link{values}} }


\author{Robert J. Hijmans}

\examples{

r <- raster(ncol=36, nrow=18)

###############################
# rasterize points
###############################

###############################
# rasterize lines
###############################

###############################
# rasterize polygons
###############################

###############################
# rasterize an extent
###############################


}

\keyword{methods}
\keyword{spatial}

