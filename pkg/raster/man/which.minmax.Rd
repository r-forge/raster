\name{which.min}

\docType{methods}

\alias{which.min}
\alias{which.max}
\alias{which.min,RasterLayer-method}
\alias{which.max,RasterLayer-method}
\alias{which.min,RasterStackBrick-method}
\alias{which.max,RasterStackBrick-method}

\title{Which cells have the minumum / maximum value (for a RasterLayer), or which layer has the minimum/maximum value (for a RasterStack or RasterBrick)?}

\usage{
which.min(x)
which.max(x)
}

\arguments{
  \item{x}{Raster* object}
}

\seealso{ \code{\link{Which}}}

\value{
vector of cell numbers (if \code{x} is a RasterLayer) or a RasterLayer (if \code{x} is a RasterStack or RasterBrick
}

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster")) 
r <- which.min(b)

i <- which.min(b[[3]])
xy <- xyFromCell(b, i)
lot(b[[3]])
points(xy)
}

\keyword{spatial}
