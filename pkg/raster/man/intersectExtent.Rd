\name{intersectExtent}

\alias{intersectExtent}
  
\title{BoundingExtent intersection}

\description{
intersectExtent returns the instersection of multiple bounding boxes
}

\usage{
intersectExtent(x, ...)
}

\arguments{
  \item{x}{A Extent or Raster* object } 
  \item{...}{ additional Extent or Raster* objects } 
}
 
\value{
a Extent object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{unionExtent}}, \code{\link[raster]{extent}},  \code{\link[raster]{setExtent}} }

\examples{
r <- raster()
b1 <- newExtent(-10, 10, -20, 20)
b2 <- newExtent(0, 20, -40, 5)
i <- intersectExtent(b1, b2)
}

\keyword{spatial}
