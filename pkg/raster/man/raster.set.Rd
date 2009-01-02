\name{set}
\alias{setRaster}
\alias{setFilename}
\alias{setBbox}
\alias{setProjection}
\alias{setDatatype}
\alias{setRowcol}
\alias{setValues}
\alias{setValuesRow}
\alias{setValuesBlock}
\alias{setValuesSparse}
\alias{clearValues}
\alias{makeSparse}
\alias{setMinmax}

\title{ Set properties of a raster object }

\description{
Use these functions to set name and datatype properties of a raster object. While you can access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 
  
}

\usage{
setRaster(raster, filename="")
setProjection(object, projection)
setFilename(object, filename)
setBbox(raster, xmn=xmin(raster), xmx=xmax(raster), ymn=ymin(raster), ymx=ymax(raster), keepres=FALSE)
setDatatype(raster, datatype, datasize)
setRowcol(raster, nrows=nrow(raster), ncols=ncol(raster)) 
setMinmax(raster)
setValues(raster, values) 
setValuesRow(raster, rowvalues, rownr) 
setValuesBlock(raster, blockvalues, firstcell, lastcell) 
setValuesSparse(raster, sparsevalues, indices) 
clearValues(raster)
makeSparse(raster)
}

\arguments{
  \item{object} {A spatial object (i.e. any Raster* objecty or Spatial* object from the SP package}
  \item{raster}{ A \code{Raster} object }
  \item{filename}{ filename of raster, can be blank }
  \item{datatype}{ the type of data ('numeric' or 'integer' that will be written to disk }
  \item{datasize}{ The size, in number of bytes, that wil be written to disk. typically 4 for integer, and 8 for numeric datatype }
  \item{values}{ Data values to associate with the raster object. There should be values for all cells }
  \item{rowvalues}{ Data values to associate with the raster object for a single row }
  \item{rownr}{ row number, between 1 and raster@nrows }
  \item{blockvalues}{ Data values to associate with the raster object for a block of data, i.e. a rectangular area within the entire raster }
  \item{firstcell}{ first cell (upper left corner) of the blockdata }
  \item{lastcell}{ last cell (upper left corner) of the blockdata }
  \item{sparsevalues}{ not implemented yet -- sparse data values. I.e. values for a number of raster cells }
  \item{indices}{ the cell numbers corresponding to the sparse data }
  \item{nrows}{ the new number of rows for the raster }
  \item{ncols}{ the new number of cols for the raster }  
  \item{xmn}{ the new minimum x coordinate of the raster }
  \item{xmx}{ the new maximum x coordinate of the raster }
  \item{ymn}{ the new minimum y coordinate of the raster }
  \item{ymx}{ the new maximum y coordinate of the raster }
  \item{keepres} {logical. If \code{TRUE}, the resolution will stay the same (by adjusting the number of rows and columns)}
  \item{projection} {a character string describing a projection and datum in PROJ4 format }
}

\details{
raster.set copies the parameters of a Raster object to a new object, but does not copy the filename nor the data associated with the original raster. 
}

\value{
	a raster object
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{ 
	rs <- newRaster()
	rs
	rs <- setFilename(rs, "c:/test.grd")
	rs <- setRowcol(rs, nrows=18, ncols=36) 
	rs
}

\keyword{ spatial }
