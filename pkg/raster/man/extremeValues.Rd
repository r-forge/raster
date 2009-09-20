\name{extremeValues}
\alias{maxValue}
\alias{minValue}
  
  
\title{Minimum and maximum values}
\description{
Returns the minimum or maximum value of a RasterLayer or layer in a RasterStack
}

\usage{
minValue(object, layer=1)
maxValue(object, layer=1)
}

\arguments{
  \item{object}{RasterLayer or RasterStack object}
  \item{layer}{layer number (for RasterStack objects) }
}
  
\value{
a number
}

\author{Robert J. Hijmans }
\examples{
r <- raster()
r <- setValues(r, 1:ncell(r))
minValue(r)
maxValue(r)
r <- setValues(r, round(100 * runif(ncell(r)) + 0.5))
minValue(r)
maxValue(r)

r <- raster(system.file("external/test.ag", package="sp"))
# this raster is from an 'external' format, so min and maxValue are not known
minValue(r)
maxValue(r)

r1 <- setMinMax(r)
# now they are known:
minValue(r1)
maxValue(r1)

r2 <- readAll(r)
# now they are known too:
minValue(r2)
maxValue(r2)


}
\keyword{spatial}
