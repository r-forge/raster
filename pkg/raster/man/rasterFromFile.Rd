\name{rasterFromFile}


\alias{rasterFromFile}
\alias{rasterFromBbox}
\alias{setRaster}

\title{Create a RasterLayer}

\description{
  These function have been depracated. Use \code{link[raster]{raster}} instead.
}

\usage{
rasterFromFile(filename, values=FALSE, band=1)
rasterFromBbox(bndbox, nrows=10, ncols=10)
setRaster(object, filename="", values=NULL)

}

\arguments{
  \item{filename}{name of raster data file  }
  \item{band}{band number in case of a file of multiple bands, default = 1 }
  \item{values}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'; or the values to associate with the RasterLayer object }
  \item{bndbox}{a BoundingBox object}
  \item{nrows}{number of rows on raster }
  \item{ncols}{number of columns on raster }
  \item{object}{A Raster* object }
}


\value{
RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\keyword{spatial }
