\name{intersectBbox}

\alias{intersectBbox}
  
\title{BoundingBox intersection}

\description{
intersectBbox returns the instersection of multiple bounding boxes
}

\usage{
intersectBbox(x, ...)
}

\arguments{
  \item{x}{A BoundingBox or Raster* object } 
  \item{...}{ additional BoundingBox or Raster* objects } 
}
 
\value{
a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{unionBbox}}, \code{\link[raster]{getBoungingBox}},  \code{\link[raster]{setBoungingBox}} }

\examples{
r <- raster()
b1 <- newBbox(-10, 10, -20, 20)
b2 <- newBbox(0, 20, -40, 5)
i <- intersectBbox(b1, b2)
}

\keyword{spatial}
