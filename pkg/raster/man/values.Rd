\name{Values}
\alias{values}
\alias{valuesRow}
\alias{cellValues}
\alias{xyValues}
\alias{cellValues,RasterLayer-method}
\alias{xyValues,RasterLayer-method}
\alias{cellValues,RasterStack-method}
\alias{xyValues,RasterStack-method}
\alias{maxValue}
\alias{minValue}
  
  
\title{ Get raster properties }
\description{
  values returns cell values for a Raster* object (typically after read* has been used). 
  valuesRow returns values for a row of values in the case that all values are in memory (if they are on disk, use readRow() and values())
  cellValues and xyValues return a vector of values for specified cells or xy coordiates
  minValue and maxValue return the extreme data values for a RasterLayer (or layer in a RasterStack or Brick)
}

\usage{
values(object, format='vector', names=FALSE)
valuesRow(raster, rownr)
cellValues(object, cells)
xyValues(object, xy)
minValue(object, layer=1)
maxValue(object, layer=1)
}

\arguments{
  \item{raster}{ RasterLayer or RasterStack object }
  \item{object}{ RasterLayer or RasterStack object }
  \item{format}{ use \code{vector} to get the data as a single vector or \code{matrix} to get them as a matrix }
  \item{names}{ If TRUE, row and col names are provided }
  \item{rownr}{ The rownumber for which values are returned }
  \item{cells}{ vector of cell numbers }
  \item{xy}{ matrix of x and y coordinates (i.e. 2 columns) }
  \item{layer}{ layer number (for RasterStack and RasterBrick objects }
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
rs <- newRaster()
rs <- setValues(rs, 1:ncells(rs))
cellValues(rs, c(1, 10, 100))
minValue(rs)
maxValue(rs)

 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))

# read values for cells
  cells <- c(1, 1900, 5000)
  cellValues(rs, cells)

# read values for (x, y) coordinates
  xy <- xyFromCell(rs, cells)
  xyValues(rs, xy)

}
\keyword{ spatial }
