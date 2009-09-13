\name{shift}

\alias{shift}
  
\title{Shift}

\description{
shift the location of a Raster* object in the x and/or y direction
}

\usage{
shift(object, x=0, y=0)
}

\arguments{
  \item{object}{A Raster* object}
  \item{x}{The shift in horizontal direction}
  \item{y}{The shift in vertical direction}
  
}
 
\value{
a Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{Extent-class}}, \code{\link[raster]{rotate}} }

\examples{
r <- raster()
r <- shift(r, x=1, y=-1)
}

\keyword{spatial}
