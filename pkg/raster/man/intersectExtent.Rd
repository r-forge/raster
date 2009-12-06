\name{intersectExtent}

\alias{intersectExtent}
  
\title{Extent intersection}

\description{
intersectExtent returns the intersection (i.e. the area in which they all overlap) of multiple Extent objects
}

\usage{
intersectExtent(x, ...)
}

\arguments{
  \item{x}{Extent object or object from which it can be coerced via \code{\link[raster]{extent}} (Raster* or Spatial* objects)} 
  \item{...}{Additional Extent or Raster* or Spatial* objects } 
}
 
\value{
Extent object
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
