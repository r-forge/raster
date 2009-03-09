\name{cellsFromBbox}

\alias{cellsFromBbox}


\title{Cells from BoundingBox}

\description{
cellsFromBbox returns the cellnumbers or a RasterLayer within a BoundingBox (rectangular area).
}

\usage{
cellsFromBbox(object, bndbox, expand=FALSE)
}

\arguments{
  \item{object}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{expand}{Logical. If \code{TRUE}, \code{NA} is returned for (virtual) cells implied by \code{bndbox}, 
      that are outside the RasterLayer (\code{object}). If \code{FALSE}, only cell numbers for the area where \code{object} and \code{bndbox}
      overlap are returned (see \link[raster]{intersectBbox} }
}

\value{
a vector of cell numbers 
}  
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{polygonFromBbox}}, \code{\link[raster]{newBbox}}, \code{\link[sp]{SpatialPolygons}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
cells <- cellsFromBbox(r, bb)
}

\keyword{spatial}


