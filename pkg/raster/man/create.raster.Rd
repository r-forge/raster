\name{raster}
\alias{raster.new}
\alias{raster.from.bbox}
\alias{raster.from.file}

\title{ Create a new RasterLayer object }
\description{
  These functions create new RasterLayer objects, either from scratch or from file.
  The created object does not contain any cell (pixel) values, it only has the parameters that describe it.
  See read* and values() for access to the values
}
\usage{
	raster.new(xmin = -180, xmax = 180, ymin = -90, ymax = 90, nrows = 180, ncols = 360, projection = "+proj=longlat +datum=WGS84")
	raster.from.bbox(boundingbox, nrows = 1, ncols = 1)
	raster.from.file(filename, values=FALSE, band=1)
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
  \item{projection}{ map projection of raster data }
  \item{boundingbox} {bounding box used to cut a raster. Any object that is or has a bounding box (see below) }
  \item{values} {logical. If \code{TRUE}, RasterLayer values will be read into memory with 'read.all()'}
}
\details{
  raster.from.file uses "GDALinfo" from the rgdal package
  see the rgdal help for supported file types
}

\value{
  raster object
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{  \code{\link[gdal]{rgdal}} }
\examples{
  raster1 <- raster.new(nrows=1080, ncols=2160)
  raster1
  raster2 <- raster.from.file(system.file("external/test.ag", package="sp"))
  raster2
}
\keyword{ spatial }
