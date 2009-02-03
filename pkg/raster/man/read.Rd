\name{Read Raster* data}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readRow,RasterLayer-method}
\alias{readRow,RasterStack-method}
\alias{readPartOfRow,RasterLayer-method}
\alias{readPartOfRow,RasterStack-method}
\alias{readBlock,RasterLayer-method}
\alias{readRows,RasterLayer-method}

\alias{readAll}
\alias{readRow}
\alias{readRows}
\alias{readPartOfRow}
\alias{readBlock}


\title{Read values from disk}
\description{Read values from a raster file associated with a RasterLayer or RasterStack object into memory.
You can either read all the data (memory permitting), or read data by row, part of row, block, by cellnumber, or for (x,y) coordinates. Data can be read from 'native raster' binary files, as well as for other formats that are supported by the rgdal package.
}


\usage{
readAll(object)
readRow(object, rownr)
readRows(object, startrow, nrows = 3)
readPartOfRow(object, rownr,  startcol=1, ncolumns=(ncol(object)-startcol+1)) 
readBlock(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) 
}

\arguments{
  \item{object}{a Raster* object}
  \item{rownr}{the row number of the row to read  (between 1 and nrows(raster))}
  \item{startrow}{the frist row to read  (>= 1 & <= nrows(raster))}
  \item{nrows}{the number of the rows to read (>= 1 & <= nrows(raster))}
  \item{startcol}{first column of the block ( (>= 1 & <= nrows(raster))}
  \item{ncolumns}{number of columns to read on this each row ( (>= 1 & <= ncols(raster) & > firstcol)}
}


\note{After read* the values are accessible with the "values(object)" function}

\author{Robert J. Hijmans}

\examples{

 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))

# read a row
  firstrow <- readRow(rs, 1)
  lastrow <- readRow(rs, rs@nrows)

# read a number of rows
  fiverows <- readRows(rs, startrow=50, nrows=5)

# read part of a row 
  rowpart <- readPartOfRow(rs, 50,  startcol=30, ncolumns=10) 

# read a block
  block <- readBlock(rs, startrow=50, nrows=5, startcol=30, ncolumns=10)
}

\keyword{classes}
\keyword{spatial}
