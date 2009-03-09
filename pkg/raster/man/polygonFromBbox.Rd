\name{polygonFromBbox}
\alias{polygonFromBbox}


\title{Polygon from BoundingBox}

\description{
polygonFromBbox creates a SpatialPolygon object from a BoundingBox object (or a Raster*) object
}

\usage{
polygonFromBbox(bndbox, sp=TRUE)
}

\arguments{
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{sp}{logical. If \code{TRUE}, the output will be a SpatialPolygons object (sp package). Otherwise a matrix of coordinates is returned}    
}

\value{
a matrix or a SpatialPolygons object
}  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{cellsFromBbox}}, \code{\link[raster]{newBbox}}, \code{\link[sp]{SpatialPolygons}} }

\examples{
r <- raster()
b <- getBbox(r)
pol <- polygonFromBbox(b)
}

\keyword{spatial}


