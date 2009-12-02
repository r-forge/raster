\name{Sample}

\alias{sampleRandom}
\alias{sampleRegular}

\title{Sample values of a RasterLayer}

\description{
Take a sample from a RasterLayer, either systematic or random
}

\usage{
sampleRandom(raster, n, na.rm = TRUE)
sampleRegular(raster, n, cells=FALSE, extent=NULL, asRaster=FALSE, corners=FALSE)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{n}{sample size}  
  \item{na.rm}{Logical. If \code{TRUE} NA values are removed from random sample}  
  \item{extent}{Extent. To limit regular sampling to the area within that box}  
  \item{cells}{Logical. Return sampled cell numbers (if asRaster==FALSE) }  
  \item{asRaster}{Logical. If \code{TRUE}, a rasterLayer is returned, rather then the sampled values}  
  \item{corners}{Logical. If \code{TRUE}, the extreme corner cells are always included. This makes for rather irregular sampling when n is small; but it can be useful when asRaster = TRUE, to maintain the same extent as the input RasterLayer}
}

\details{
Sample size is approximate for sampleRegular and for sampleRandom with na.rm=TRUE
}

\author{Robert J. Hijmans}

\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
 v <- sampleRegular(r, n=100)
}

\keyword{spatial}
