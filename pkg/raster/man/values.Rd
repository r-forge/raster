\name{Values}
\alias{values}
\alias{valuesRow}
\alias{maxValue}
\alias{minValue}
  
  
\title{Get raster properties}
\description{
  values returns cell values for a Raster* object (typically after read* has been used). 
  valuesRow returns values for a row of values in the case that all values are in memory (if they are on disk, use readRow() and values())
  minValue and maxValue return the extreme data values for a RasterLayer (or layer in a RasterStack)
}

\usage{
values(object, format='vector', names=FALSE)
valuesRow(raster, rownr)
minValue(object, layer=1)
maxValue(object, layer=1)
}

\arguments{
  \item{raster}{RasterLayer or RasterStack object}
  \item{object}{RasterLayer or RasterStack object}
  \item{format}{use \code{vector} to get the data as a single vector or \code{matrix} to get them as a matrix}
  \item{names}{If TRUE, row and col names are provided}
  \item{rownr}{The rownumber for which values are returned}
  \item{layer}{layer number (for RasterStack objects}
}
  

\note{
	RasterLayer values are returned as a vector unless the option format='matrix' is used. In that case, a matrix is returned, optionally with the row and col numbers (names=TRUE). 
	Some of these functions simply take the values from the data slots in the Raster or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
  
}

\value{
raster cell value(s)
}

\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
r <- newRaster()
r <- setValues(r, 1:ncell(r))
minValue(r)
maxValue(r)
r <- setValues(r, round(100 * runif(ncell(r)) + 0.5))
minValue(r)
maxValue(r)

r <- rasterFromFile(system.file("external/test.ag", package="sp"))
# this raster is from an 'external' format, so min and maxValue are not known
minValue(r)
maxValue(r)

r1 <- setMinMax(r, TRUE)
# now they are known:
minValue(r1)
maxValue(r1)

r2 <- readAll(r)
# now they are known too:
minValue(r2)
maxValue(r2)


}
\keyword{spatial}
