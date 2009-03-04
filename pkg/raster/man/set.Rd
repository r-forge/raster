\name{set}
\alias{setRaster}
\alias{setFilename}
\alias{filename<-}
\alias{setProjection}
\alias{setRowCol}
\alias{ncol<-}
\alias{nrow<-}
\alias{setRes}
\alias{resolution<-}
\alias{setMinMax}
\alias{setNAvalue}

\title{ Set properties of a RasterLayer object }

\description{
Use these functions to set certain properties of a RasterLayer object. 
}

\usage{
setProjection(object, projstring)
setFilename(object, filename)
setRaster(object, filename="", values=NULL)
setRowCol(raster, nrows=nrow(raster), ncols=ncol(raster)) 
setRes(object, xres, yres=xres)
setMinMax(raster, readfromdisk=FALSE)
setNAvalue(raster, value)
}

\arguments{
  \item{object}{A Raster* object }
  \item{raster}{ A \code{RasterLayer} object }
  \item{values}{ Data values to associate with the RasterLayer object }
  \item{filename}{ filename, can be blank }
  \item{nrows}{ the new number of rows for the raster }
  \item{ncols}{ the new number of cols for the raster }  
  \item{xres}{ the new x resolution}
  \item{yres}{ the new y resolution}  
  \item{projstring}{a character string describing a projection and datum in PROJ4 format }
  \item{readfromdisk}{Logical. If \code{TRUE} then data are read from disk (if available) to determine the min and max value}
  \item{value}{the value to be interpreted as NA; set this before reading the values from the file. Integer values are matched exactely; for decimal values files any value <= the value will be interpreted as NA}  
}

\details{
setRaster copies the parameters of a RasterLayer object to a new object, but does not copy the filename nor the data associated with the original raster. 
Use it when creating new RasterLayer objects from existing objects.


While you could access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 
}

\value{
	a Raster* object
}
\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
r <- setRowCol(r, nrows=18, ncols=36) 
# equivalent to
nrow(r) <- 18
ncol(r) <- 36
r <- setRes(r, xres=1/120) 
# equivalent to
resolution(r) <- 1/120
# set yres diffrently
resolution(r) <- c(1/120, 1/60)
r <- setProjection(r, "+proj=longlat +datum=WGS84")
r <- setFilename(r, "c:/test.grd")
	
}

\keyword{ spatial }

