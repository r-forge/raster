\name{brick create}
\alias{newBrick}
\alias{brickFromBbox}
\alias{brickFromFile}
\alias{brickFromStack}

\title{ Create a new RasterBrick object }
\description{
  These functions create new RasterBrick objects, either from scratch or from file.
  The created object does not contain any cell (pixel) values, it only has the parameters that describe it.
  See read* and values() for access to the values
}
\usage{
	newBrick(xmn = -180, xmx = 180, ymn = -90, ymx = 90, nrows = 180, ncols = 360, projection = "+proj=longlat +datum=WGS84")
	brickFromBbox(boundingbox, nrows = 1, ncols = 1)
	brickFromFile(filename, values=FALSE)
	brickFromStack(stack, values=FALSE)
}

\arguments{
  \item{filename}{ name of raster data file  }
  \item{xmn}{ minimum x coordinate or raster (left border) }
  \item{xmx}{ maximum x coordinate of raster (right border) }
  \item{ymn}{ minimum y coordinate or raster (bottom border) }
  \item{ymx}{ maximum y coordinate of raster (top border) }
  \item{nrows}{ number of rows on raster }
  \item{ncols}{ number of columns on raster }
  \item{projection}{ map projection of raster data }
  \item{boundingbox} {bounding box used to crop (cut, subset) a raster. Any object that is or has a bounding box (see below) }
  \item{values} {logical. If \code{TRUE}, RasterBrick values will be read into memory with 'readAll()'}
  \item{stack}{ RasterStack object }
 }
\details{
  rasterFromFile uses "GDALinfo" from the rgdal package
  see the rgdal help for supported file types
}

\value{
  RasterBrick object
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{  \code{\link[gdal]{rgdal}} }
\examples{
  b1 <- newBrick(nrows=1080, ncols=2160)
  b1
}
\keyword{ spatial }
