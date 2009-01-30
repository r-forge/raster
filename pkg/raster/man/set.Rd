\name{set}
\alias{setRaster}
\alias{setFilename}
\alias{setProjection}
\alias{setDatatype}
\alias{setRowCol}
\alias{setRes}
\alias{setMinMax}

\title{ Set properties of a RasterLayer object }

\description{
Use these functions to set name and datatype properties of a RasterLayer object. While you can access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 
}

\usage{
setProjection(object, projstring)
setFilename(object, filename)
setRaster(object, filename="", values=NA)
setDatatype(raster, datatype, datasize)
setRowCol(raster, nrows=nrow(raster), ncols=ncol(raster)) 
setRes(object, xres, yres=xres)
setMinMax(raster, readfromdisk=FALSE)
}

\arguments{
  \item{object}{A Raster* object }
  \item{raster}{ A \code{RasterLayer} object }
  \item{values}{ Data values to associate with the RasterLayer object}
  \item{filename}{ filename, can be blank }
  \item{datatype}{ the type of data ('numeric' or 'integer' that will be written to disk }
  \item{datasize}{ The size, in number of bytes, that wil be written to disk. typically 4 for integer, and 8 for numeric datatype }
  \item{nrows}{ the new number of rows for the raster }
  \item{ncols}{ the new number of cols for the raster }  
  \item{xres}{ the new x resolution}
  \item{yres}{ the new y resolution}  
  \item{projstring}{a character string describing a projection and datum in PROJ4 format }
  \item{readfromdisk}{Logical. If \code{TRUE} then data are read from disk (if available) to determine the min and max value}
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
	r <- raster()
	r <- setFilename(r, "c:/test.grd")
	r <- setRowCol(r, nrows=18, ncols=36) 
	r <- setRes(r, xres=1/120) 
	r <- setProjection(r, "+proj=longlat +datum=WGS84")
	r <- setDatatype(r, datatype="integer", datasize=4)
	r <- setDatatype(r, datatype="numeric", datasize=8)
}

\keyword{ spatial }

