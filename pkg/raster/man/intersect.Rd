\name{intersect}

\docType{methods}

\alias{intersect,Extent,Extent-method}

\title{ 
Intersect Extent
}

\description{
Intersect two Extent objects. Returns the intersection, i.e. the area of overlap of two Extent objects.


Use \code{\link{crop}} if you want to intersect a Spatial* object with an Extent object.
}

\usage{
\S4method{intersect}{Extent,Extent}(x, y)
}

\arguments{
  \item{x}{Extent}
  \item{y}{Extent}
} 

\value{
Extent
}

\author{Robert J. Hijmans}

\seealso{
\code{\link{unionExtent}, \link{intersectExtent}, \link[raster]{extent}}
}

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersect(e1, e2)
}


\keyword{methods}
\keyword{spatial}

