\name{fromBbox}
\alias{cellsFromBbox}
\alias{polygonFromBbox}


\title{Additional BoundingBox functions}
\description{
cellsFromBbox returns the cellnumbers or a RasterLayer within a BoundingBox (rectangular area).
polygonFromBbox creates a SpatialPolygon object from a BoundingBox object (or a Raster*) object
You can also use bbox(object) to get a sp type bbox (a 2 x 2 matrix), but such an object cannot be used with the other functon described here as it returns a matrix, and not a BoundingBox object.
}

\usage{
cellsFromBbox(object, bndbox)
polygonFromBbox(bndbox, sp=TRUE)
}

\arguments{
  \item{object}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{sp}{logical. If \code{TRUE}, the output will be a SpatialPolygons object (sp package). Otherwise a matrix of coordinates is returned}    
}

\value{
a vector of cell numbers of a SpatialPolygons object
}  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{newBbox}}, \code{\link[sp]{SpatialPolygons}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
cells <- cellsFromBbox(r, bb)
pol <- polygonFromBbox(bb)
}

\keyword{spatial}


