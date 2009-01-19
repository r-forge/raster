\name{raster}
\alias{newRaster}
\alias{rasterFromBbox}
\alias{rasterFromFile}
\alias{closeHandle}

\title{ Create a new RasterLayer object }
\description{
  These functions create new RasterLayer objects, either from scratch or from file.
  The created object does not contain any cell (pixel) values, it only has the parameters that describe it.
  See read* and values() for access to the values
}
\usage{
	newRaster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, nrows = 180, ncols = 360, projstring = "+proj=longlat +datum=WGS84")
	rasterFromBbox(bndbox, nrows = 1, ncols = 1, projstring="")
	rasterFromFile(filename, values=FALSE, band=1)
	closeHandle(raster)
}

\arguments{
  \item{filename}{ name of raster data file  }
  \item{band}{ band number in case of a file of multiple bands, default = 1 }
  \item{xmn}{ minimum x coordinate or raster (left border) }
  \item{xmx}{ maximum x coordinate of raster (right border) }
  \item{ymn}{ minimum y coordinate or raster (bottom border) }
  \item{ymx}{ maximum y coordinate of raster (top border) }
  \item{nrows}{ number of rows on raster }
  \item{ncols}{ number of columns on raster }
  \item{projstring}{ PROJ4 type description of a map projection}
  \item{bndbox}{bounding box used to crop a raster. Any object that is or has a bounding box (see below) }
  \item{values}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'}
  \item{raster}{a RasterLayer object}
}
\details{
  rasterFromFile uses "GDALinfo" from the rgdal package
  see the rgdal help for supported file types
  closeHandle removes the handle to the file on disk that was established by rasterFromFile(). After the handle is closed, you can no longer read data from the file through the RasterLayer object. You probably do not need to do this; unless you get problems with files that cannot be overwritten (rm() does not immediately remove the handle; it seems). 
}

\value{
  RasterLayer object
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{  \code{\link[gdal]{rgdal}} }
\examples{
  r1 <- newRaster(nrows=1080, ncols=2160)
  r1
  r2 <- rasterFromFile(system.file("external/test.ag", package="sp"))
  r2
  logo <- rasterFromFile(system.file("pictures/Rlogo.jpg", package="rgdal"))
}
\keyword{ spatial }
