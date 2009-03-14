\name{rasterFromFile}

\alias{rasterFromFile}
\alias{rasterFromBbox}

\title{Raster from file}

\description{
  These function have been depracated. Use \code{link[raster]{raster}} instead.
}

\usage{
rasterFromFile(filename, values=FALSE, band=1)
rasterFromBbox(bndbox, nrows=10, ncols=10)

}

\arguments{
  \item{filename}{name of raster data file  }
  \item{band}{band number in case of a file of multiple bands, default = 1 }
  \item{values}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'}
  \item{bndbox}{a BoundingBox object}
  \item{nrows}{number of rows on raster }
  \item{ncols}{number of columns on raster }
}


\value{
RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\keyword{spatial }
