\name{rasterFromFile}

\alias{rasterFromFile}

\title{RasterLayer from file}

\description{
  Create a new RasterLayer object from a file. The created object does normally not contain any cell (pixel) values, 
  it only has the parameters that describe the RasterLayer.
}

\usage{
rasterFromFile(filename, values=FALSE, band=1)
}

\arguments{
  \item{filename}{name of raster data file  }
  \item{band}{band number in case of a file of multiple bands, default = 1 }
  \item{values}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'}
}

\details{
 Supported file types are the 'native' raster package format and those that can be read via rgdal. See ?readGDAL help for supported file types.
 See read* and values() for access to the values. 
}

\value{
RasterLayer object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{readAll}, \link[raster]{values},  \link[raster]{raster}, \link[raster]{stack},  \link[gdal]{readGDAL} } }
\examples{
r <- rasterFromFile(system.file("external/test.ag", package="sp"))
logo <- rasterFromFile(system.file("pictures/Rlogo.jpg", package="rgdal"))
}

\keyword{spatial }
