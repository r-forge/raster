\name{read}
\alias{read.all}
\alias{read.row}
\alias{read.rows}
\alias{read.part.of.row}
\alias{read.block}


\title{ Read values from a file into memory}
\description{
  Data associated with a Raster or RasterStackobject are read from file. Either all the data, by row, part of row, block, by cellnumber, or for (x,y) coordinates. Data can be read for 'raster' binary files, as well as for other formats that are supported in rgdal.
  read with read.all, read.row, read.part.of.row, read.block
}

\usage{
read.all(object)
read.row(object, rownr)
read.rows(object, startrow, nrows = 3)
read.part.of.row(object, rownr,  startcol=1, ncolumns=(ncol(object)-startcol+1)) 
read.block(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) 

}

\arguments{
  \item{object}{ a Raster or RasterStack object }
  \item{rownr}{ the row number of the row to read  (between 1 and raster@nrows) }
  \item{startrow}{ the frist row to read  (>= 1 & <= raster@nrows) }
  \item{nrows}{ the number of the rows to read (>= 1 & <= raster@nrows) }
  \item{startcol}{ first column of the block ( (>= 1 & <= raster@ncols) }
  \item{ncolumns}{ number of columns to read on this each row ( (>= 1 & <= raster@ncols & > firstcol) }
}
\details{
}
\value{
  a raster object containing a vector with raster values or a rasterstack object with a matrix of values.  
  or raster values for a set of points
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{

 rs <- raster.from.file(system.file("external/test.ag", package="sp"))

# read a row
  firstrow <- read.row(rs, 1)
  lastrow <- read.row(rs, rs@nrows)

# read a number of rows
  fiverows <- read.rows(rs, startrow=50, nrows=5)

# read part of a row 
  rowpart <- read.part.of.row(rs, 50,  startcol=30, ncolumns=10) 

# read a block
  block <- read.block(rs, startrow=50, nrows=5, startcol=30, ncolumns=10) 
}
  
\keyword{ spatial }
