\name{Sample}

\alias{sampleRandom}
\alias{sampleSkip}

\title{Sample values of a RasterLayer}

\description{
read a sample from a RasterLayer, either systematic or random
}

\usage{
sampleSkip(raster, maxdim=500, bndbox=NA, asRaster=FALSE)
sampleRandom(raster, n=500, na.rm = TRUE)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{maxdim}{sets the density for regular sampling}  
  \item{bndbox}{sets a bouding box within the raster, for regular sampling within that box}  
  \item{asRaster}{Logical. If \code{TRUE}, a rasterLayer is returned, rather then the sampled values}  
  \item{n}{size of random sample}  
  \item{na.rm}{Logical. If \code{TRUE} NA values are removed from random sample}  
}


\author{Robert J. Hijmans}

\examples{
 r <- raster(system.file("external/test.ag", package="sp"))
 v <- sampleSkip(r, 10)
}

\keyword{spatial}
