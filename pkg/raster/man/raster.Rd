\name{raster}

\alias{raster}
\alias{raster,missing-method}
\alias{raster,Raster-method}
\alias{raster,character-method}
\alias{raster,BoundingBox-method}


\title{Create a new RasterLayer object }

\description{
  These functions create new RasterLayer objects
  The created object does normally not contain any cell (pixel) values, it only has the parameters that describe the RasterLayer.
}

\usage{
raster(x, ...)
}

\arguments{
  \item{x}{ many things... }
  \item{...}{ even more things... }  
}

\details{
raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projstring="+proj=longlat +datum=WGS84")
raster(bndbox, nrows=10, ncols=10)
raster(filename)
raster(Raster* object)

  \item{xmn}{minimum x coordinate or raster (left border) }
  \item{xmx}{maximum x coordinate of raster (right border) }
  \item{ymn}{minimum y coordinate or raster (bottom border) }
  \item{ymx}{maximum y coordinate of raster (top border) }
  \item{nrows}{number of rows on raster }
  \item{ncols}{number of columns on raster }
  \item{projstring}{PROJ4 type description of a map projection}
  \item{bndbox}{bounding box used to crop a raster. Any object that is a BoundingBox object or contains one (such as Raster* objects)}



New RasterLayer objects have no values. You can set values with the \code{init} function or with a 'replacement function' (see example)
}

\value{
RasterLayer object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{setValues},  \link[raster]{init}, \link[raster]{newBbox},  \link[raster]{stack}} }

\examples{
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
bb <- getBbox(r1)
r2 <- raster(bb)
r2[] <- runif(ncell(r2))
}

\keyword{spatial }
