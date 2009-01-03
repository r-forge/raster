\name{raster}
\alias{newRaster}
\alias{rasterFromBbox}
\alias{rasterFromFile}

\title{ Create a new RasterLayer object }
\description{
  These functions create new RasterLayer objects, either from scratch or from file.
  The created object does not contain any cell (pixel) values, it only has the parameters that describe it.
  See read* and values() for access to the values
}
\usage{
	newRaster(xmin = -180, xmax = 180, ymin = -90, ymax = 90, nrows = 180, ncols = 360, projstring = "+proj=longlat +datum=WGS84")
	rasterFromBbox(boundingbox, nrows = 1, ncols = 1)
	rasterFromFile(filename, values=FALSE, band=1)
}

\arguments{
  \item{filename}{ name of raster data file  }
  \item{band}{ band number in case of a file of multiple bands, default = 1 }
  \item{xmin}{ minimum x coordinate or raster (left border) }
  \item{xmax}{ maximum x coordinate of raster (right border) }
  \item{ymin}{ minimum y coordinate or raster (bottom border) }
  \item{ymax}{ maximum y coordinate of raster (top border) }
  \item{nrows}{ number of rows on raster }
  \item{ncols}{ number of columns on raster }
  \item{projstring}{ map projection of raster data, formatted as a PROJ4 string }
  \item{boundingbox} {bounding box used to crop a raster. Any object that is or has a bounding box (see below) }
  \item{values} {logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'}
}
\details{
  rasterFromFile uses "GDALinfo" from the rgdal package
  see the rgdal help for supported file types
}

\value{
  RasterLayer object
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{  \code{\link[gdal]{rgdal}} }
\examples{
  raster1 <- newRaster(nrows=1080, ncols=2160)
  raster1
  raster2 <- rasterFromFile(system.file("external/test.ag", package="sp"))
  raster2
}
\keyword{ spatial }
