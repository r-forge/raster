\name{Raster read functions}
\docType{class}
\alias{readAll}
\alias{readRow}
\alias{readRows}
\alias{readPartOfRow}
\alias{readBlock}
\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readRow,RasterLayer-method}
\alias{readRow,RasterStack-method}
\alias{readPartOfRow,RasterLayer-method}
\alias{readPartOfRow,RasterStack-method}
\alias{readBlock,RasterLayer-method}
\alias{readRows,RasterLayer-method}

\title{Read values from disk }
\description{ Read values from a raster type file associated with a RasterLayer, RasterStack or RasterBrick object into memory.
You can either read all the data (memory permitting), or read data by row, part of row, block, by cellnumber, or for (x,y) coordinates. Data can be read from 'native raster' binary files, as well as for other formats that are supported by GDAL.
}


\usage{
readAll(object)
readRow(object, rownr)
readRows(object, startrow, nrows = 3)
readPartOfRow(object, rownr,  startcol=1, ncolumns=(ncol(object)-startcol+1)) 
readBlock(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) 

}

\arguments{
  \item{object}{ a Raster or RasterStack object }
  \item{rownr}{ the row number of the row to read  (between 1 and raster@nrows) }
  \item{startrow}{ the frist row to read  (>= 1 & <= raster@nrows) }
  \item{nrows}{ the number of the rows to read (>= 1 & <= raster@nrows) }
  \item{startcol}{ first column of the block ( (>= 1 & <= raster@ncols) }
  \item{ncolumns}{ number of columns to read on this each row ( (>= 1 & <= raster@ncols & > firstcol) }
}



\section{Methods}{
  \describe{
	\item(readAll) {\code{signature(object = "RasterLayer")}: reads all values of a RasterLayer into memory }
	\item(readAll) {\code{signature(object = "RasterStack")}: reads all values of a RasterStack into memory }
	\item(readRow) {\code{signature(object = "RasterLayer")}: reads values for a single row of a RasterLayer into memory }
	\item(readRow) {\code{signature(object = "RasterStack")}: reads values for a single row if a RasterStack into memory }
 	 }
}

\note{ After read* the values are accessible with the "values(object)" function }

\references{ ~put references to the literature/web site here ~ }
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

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
