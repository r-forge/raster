\name{set}
\alias{setRaster}
\alias{setFilename}
\alias{setProjection}
\alias{setDatatype}
\alias{setRowCol}
\alias{setValues}
\alias{setValuesBlock}
\alias{setValuesSparse}
\alias{clearValues}
\alias{makeSparse}
\alias{setMinmax}

\title{ Set properties of a RasterLayer object }

\description{
Use these functions to set name and datatype properties of a RasterLayer object. While you can access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 
  
}

\usage{
setRaster(raster, filename="")
setProjection(object, projstring)
setFilename(object, filename)
setDatatype(raster, datatype, datasize)
setRowCol(raster, nrows=nrow(raster), ncols=ncol(raster)) 
setMinmax(raster)
setValues(raster, values, rownr=-1) 
setValuesBlock(raster, blockvalues, firstcell, lastcell) 
setValuesSparse(raster, sparsevalues, indices) 
clearValues(raster)
makeSparse(raster)
}

\arguments{
  \item{object} {A Raster* object }
  \item{raster}{ A \code{RasterLayer} object }
  \item{filename}{ filename, can be blank }
  \item{datatype}{ the type of data ('numeric' or 'integer' that will be written to disk }
  \item{datasize}{ The size, in number of bytes, that wil be written to disk. typically 4 for integer, and 8 for numeric datatype }
  \item{values}{ Data values to associate with the RasterLayer object. There should be values for all cells or for a single row}
  \item{rownr}{ row number, between 1 and nrow(raster) }
  \item{blockvalues}{ Data values to associate with the RasterLayer object for a block of data, i.e. a rectangular area within the entire raster }
  \item{firstcell}{ first cell (upper left corner) of the blockdata }
  \item{lastcell}{ last cell (upper left corner) of the blockdata }
  \item{sparsevalues}{ not implemented yet -- sparse data values. I.e. values for a number of raster cells }
  \item{indices}{ the cell numbers corresponding to the sparse data }
  \item{nrows}{ the new number of rows for the raster }
  \item{ncols}{ the new number of cols for the raster }  
  \item{projstring} {a character string describing a projection and datum in PROJ4 format }
}

\details{
setRaster copies the parameters of a RasterLayer object to a new object, but does not copy the filename nor the data associated with the original raster. 
Use it when creating new RasterLayer objects from existing objects.
}

\value{
	a Raster* object
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{ 
	rs <- newRaster()
	rs <- setFilename(rs, "c:/test.grd")
	rs <- setRowCol(rs, nrows=18, ncols=36) 
	rs <- setProjection(rs, "+proj=longlat +datum=WGS84")
	rs <- setDatatype(rs, datatype="integer", datasize=4)
	rs <- setDatatype(rs, datatype="numeric", datasize=8)
	vals <- 1:ncells(rs)
	rs <- setValues(rs, vals)
	rs <- clearValues(rs)
	rowvals <- 1:ncol(rs)
	rs <- setValues(rs, rowvals, 3)
	
}

\keyword{ spatial }

