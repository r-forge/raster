\name{read}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readAll,RasterBrick-method}
\alias{readRow,RasterLayer-method}
\alias{readRow,RasterStack-method}
\alias{readRow,RasterBrick-method}

\alias{readAll}
\alias{readRow}

\title{Read values from disk}

\description{
Read values from a raster file associated with a Raster* object into memory. These functions will be removed soon. You should use \code{\link[raster]{getValues}} or \code{\link[raster]{getValuesBlock}} in stead. You can either read all the data (memory permitting), or read data for a single or multiple rows, or a block (a rectangular area). 
read* functions will fail when there is no file associated with the RasterLayer (values may only exist on memory). It is therefore safer to use
\code{\link[raster]{getValues}}.
}


\usage{
readAll(object)
readRow(object, rownr)
}


\arguments{
  \item{object}{a Raster* object}
  \item{rownr}{the row number of the row to read  (between 1 and nrows(raster))}
}


\note{
After read* the values are accessible with the "values(object)" function
Functions readRows and readBlock were are (for the time being) still available as raster:::..readRows and raster:::..readBlock
}

\seealso{ \code{\link[raster]{getValues}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{cellValues}}, \code{\link[raster]{writeRaster}}}

\author{Robert J. Hijmans}

\examples{

 rs <- raster(system.file("external/test.grd", package="raster"))

# read a row
  firstrow <- readRow(rs, 1)
  lastrow <- readRow(rs, rs@nrows)

# read a row from a stack
  st <- stack(rs, rs, rs)  
  st <- readRow(st, 50)
}

\keyword{classes}
\keyword{spatial}
