\name{coordinates}

\alias{xmin}
\alias{xmax}
\alias{ymin}
\alias{ymax}
\alias{origin}

  
\title{coordinates}

\description{
These functions return the extreme coordinates of a Raster* object. 

Origin returns the coordinates of the point of origin of a Raster* object. This is the point closest to (0, 0) 
that you could get if you moved towards that point in steps of the x and y resolution.
}

\usage{
xmin(object)
xmax(object)
ymin(object)
ymax(object)
origin(object)
}

\arguments{
  \item{object}{Raster* object}
}

\note{
  Some of these functions simply take the values from the data slots in the RasterLayer or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\value{
  a single number or (for \code{origin}, a vector of two numbers.
}

\author{Robert J. Hijmans }

\examples{

r <- raster(xmn=-0.5, xmx = 9.5, ncols=10)
xmin(r)
xmax(r)
ymin(r)
ymax(r)
origin(r)

}

\keyword{spatial}
