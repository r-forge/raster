\name{rotate}

\alias{rotate}
  
\title{Rotate}

\description{
Rotate a RasterLayer that has x coordinates (longitude) from 0 to 360, to standard coordinates between -180 and 180 degrees. Longitude between 0 and 360 is frequently used in climate data (e.g. IPCC climate change data)
}

\usage{
rotate(raster)
}

\arguments{
  \item{raster}{a RasterLayer object}
}

\value{
a RasterLayer
}

\seealso{ \code{\link[raster]{flip}} }

\author{Robert J. Hijmans }
\examples{

r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
r[] <- as.vector(t(m))
extent(r) <- extent(0, 360, -90, 90)
rr <- rotate(r)
}

\keyword{spatial}
