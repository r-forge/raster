\name{new RasterLayer}
\alias{raster}
\alias{rasterFromBbox}

\title{Create a new RasterLayer object }
\description{
  These functions create new RasterLayer objects
  The created object does normally not contain any cell (pixel) values, it only has the parameters that describe the RasterLayer.
}

\usage{
raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, nrows = 180, ncols = 360, projstring = "+proj=longlat +datum=WGS84")
rasterFromBbox(bndbox, nrows = 10, ncols = 10, nudge=TRUE)
}

\arguments{
  \item{xmn}{minimum x coordinate or raster (left border) }
  \item{xmx}{maximum x coordinate of raster (right border) }
  \item{ymn}{minimum y coordinate or raster (bottom border) }
  \item{ymx}{maximum y coordinate of raster (top border) }
  \item{nrows}{number of rows on raster }
  \item{ncols}{number of columns on raster }
  \item{nudge}{logical. If \code{TRUE}, the boundingbox is nudged outwards (to the next integer) }
  \item{projstring}{PROJ4 type description of a map projection}
  \item{bndbox}{bounding box used to crop a raster. Any object that is a BoundingBox object or contains one (such as Raster* objects)}
}

\details{
New RasterLayer objects have no values. You can set values with the \code{init} function or with a 'replacement function' (see example)
}

\value{
RasterLayer object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{setValues},  \link[raster]{init}, \link[raster]{newBbox},  \link[raster]{stack}} }
\examples{
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
r2 <- rasterFromBbox(r1)
r2[] <- runif(ncell(r2))
}

\keyword{spatial }
