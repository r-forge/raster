\name{unionExtent}

\alias{unionExtent}
  
\title{BoundingBox union}

\description{
unionExtent returns the union of multiple bounding boxes
}

\usage{
unionExtent(x, ...)
}

\arguments{
  \item{x}{A BoundingBox or Raster* object } 
  \item{...}{ additional BoundingBox or Raster* objects } 
}
 
\value{
a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{intersectExtent}}, \code{\link[raster]{extent}},  \code{\link[raster]{setExtent}},  \code{\link[raster]{newExtent}} }

\examples{
r <- raster()
b1 <- newExtent(-10, 10, -20, 20)
b2 <- newExtent(0, 20, -40, 5)
u <- unionExtent(b1, b2)
}

\keyword{spatial}
