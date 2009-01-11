\name{Raster read functions}
\docType{class}
\alias{readAll}
\alias{readRow}
\alias{readRows}
\alias{readPartOfRow}
\alias{readBlock}
\alias{readRandom}
\alias{readSkip}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readRow,RasterLayer-method}
\alias{readRow,RasterStack-method}
\alias{readPartOfRow,RasterLayer-method}
\alias{readPartOfRow,RasterStack-method}
\alias{readBlock,RasterLayer-method}
\alias{readRows,RasterLayer-method}

\title{Read values from disk }
\description{ Read values from a raster file associated with a RasterLayer, RasterStack or RasterBrick object into memory.
You can either read all the data (memory permitting), or read data by row, part of row, block, by cellnumber, or for (x,y) coordinates. Data can be read from 'native raster' binary files, as well as for other formats that are supported by the rgdal package.
}


\usage{
readAll(object)
readRow(object, rownr)
readRows(object, startrow, nrows = 3)
readPartOfRow(object, rownr,  startcol=1, ncolumns=(ncol(object)-startcol+1)) 
readBlock(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) 
readSkip(raster, maxdim=500, bndbox=NA, asRaster=FALSE)
readRandom(raster, n=500, na.rm = TRUE)
}

\arguments{
  \item{object}{ a Raster* object }
  \item{raster}{ a RasterLayer object }
  \item{rownr}{ the row number of the row to read  (between 1 and nrows(raster)) }
  \item{startrow}{ the frist row to read  (>= 1 & <= nrows(raster)) }
  \item{nrows}{ the number of the rows to read (>= 1 & <= nrows(raster)) }
  \item{startcol}{ first column of the block ( (>= 1 & <= nrows(raster)) }
  \item{ncolumns}{ number of columns to read on this each row ( (>= 1 & <= ncols(raster) & > firstcol) }
  \item{maxdim}{ sets the density for regular sampling }  
  \item{bndbox}{ sets a bouding box within the raster, for regular sampling within that box }  
  \item{asRaster}{ Logical. If \code{TRUE}, a rasterLayer is returned, rather then the sampled values}  
  \item{n}{ size of random sample }  
  \item{na.rm}{Logical. If \code{TRUE} NA values are removed from random sample }  
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
