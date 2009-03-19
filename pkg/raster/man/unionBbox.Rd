\name{unionBbox}

\alias{unionBbox}
  
\title{BoundingBox union}

\description{
unionBbox returns the union of multiple bounding boxes
}

\usage{
unionBbox(x, ...)
}

\arguments{
  \item{x}{A BoundingBox or Raster* object } 
  \item{...}{ additional BoundingBox or Raster* objects } 
}
 
\value{
a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{intersectBbox}}, \code{\link[raster]{extent}},  \code{\link[raster]{setExtent}} }

\examples{
r <- raster()
b1 <- newBbox(-10, 10, -20, 20)
b2 <- newBbox(0, 20, -40, 5)
u <- unionBbox(b1, b2)
}

\keyword{spatial}
