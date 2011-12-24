\name{unionExtent}

\alias{unionExtent}
  
\title{Extent union}

\description{
unionExtent returns the union of multiple Extent objects
}

\usage{
unionExtent(x, ...)
}

\arguments{
  \item{x}{A Extent or Raster* object } 
  \item{...}{ additional Extent or Raster* objects } 
}
 
\value{
a Extent object
}
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{intersectExtent}}, \code{\link[raster]{extent}},  \code{\link[raster]{setExtent}}} 

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
unionExtent(e1, e2)
}

\keyword{spatial}
