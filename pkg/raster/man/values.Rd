\name{properties}
\alias{values}
\alias{values.row}
\alias{valuesCells}
\alias{valuesXY}
\alias{valuesCells,RasterLayer-method}
\alias{valuesXY,RasterLayer-method}
\alias{valuesCells,RasterStack-method}
\alias{valuesXY,RasterStack-method}
\alias{maxValue}
\alias{minValue}
\alias{dataIndices}
\alias{dataContent}
\alias{dataSource}


  
  
\title{ Get raster properties }
\description{
  values returns data values that are associated with the raster object (e.g. after read.* has been used). RasterLayer data are returned as a vector unless the option format='matrix' is used. In that case, a matrix is returned, optionally with the row and col numbers (names=TRUE). 
}

\usage{
values(object, format='vector', names=FALSE)
values.row(raster, rownr)
valuesCells(object, cells)
valuesXY(object, xy)
minValue(raster)
maxValue(raster)
dataContent(raster)
dataIndices(raster)
dataSource(raster)
}

\arguments{
  \item{raster}{ RasterLayer or RasterStack object }
  \item{object}{ RasterLayer or RasterStack object }
  \item{format}{ use \code{vector} to get the data as a single vector or \code{matrix} to get them as a matrix }
  \item{names}{ If TRUE, row and col names are provided }
  \item{rownr}{ The rownumber for which values are returned }
  \item{cells}{ vector of cell numbers }
  \item{xy}{ matrix of x and y coordinates (i.e. 2 columns) }
 }
  

\note{
  Some of these functions simply take the values from the data slots in the Raster or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\value{
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
rs <- raster.new()
minValue(rs)
maxValue(rs)
dataContent(rs)
dataIndices(rs)
rs <- set.values(rs, 1:ncells(rs))
valuesCells(rs, c(1, 10, 100))
minValue(rs)
maxValue(rs)
dataSource(rs)
dataContent(rs)
dataIndices(rs)


 rs <- raster.from.file(system.file("external/test.ag", package="sp"))

# read values for cells
  cells <- c(1, 1900, 5000)
  valuesCells(rs, cells)

# read values for (x, y) coordinates
  xy <- get.xy.from.cell(rs, cells)
  valuesXY(rs, xy)

}
\keyword{ spatial }
