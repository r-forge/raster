\name{rasterFromFile}


\alias{rasterFromFile}

\title{Create a RasterLayer}

\description{
  This function has been depracated. Use \code{link[raster]{raster}} instead.
}

\usage{
rasterFromFile(filename, values=FALSE, band=1)

}

\arguments{
  \item{filename}{name of raster data file  }
  \item{band}{band number in case of a file of multiple bands, default = 1 }
  \item{values}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'; or the values to associate with the RasterLayer object }
}


\value{
A RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\keyword{spatial }
