\name{unionBoundingBox}

\alias{unionBbox}
\alias{intersectBbox}
  
\title{BoundingBox union and intersection}

\description{
unionBbox returns the union of multiple bounding boxes
intersectBbox returns the instersection of multiple bounding boxes
}

\usage{
unionBbox(x, ...)
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

\seealso{ \code{\link[raster]{getBoungingBox}},  \code{\link[raster]{setBoungingBox}} }

\examples{
r <- raster()
b1 <- newBbox(-10, 10, -20, 20)
b2 <- newBbox(0, 20, -40, 5)
u <- unionBbox(b1, b2)
i <- intersectBbox(b1, b2)
}

\keyword{spatial}
