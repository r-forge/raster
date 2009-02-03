\name{Summary-methods}
\docType{methods}
\alias{sum,Raster-method}
\alias{max,Raster-method}
\alias{min,Raster-method}
\alias{range,Raster-method}

\title{ Summary methods for Raster* objects }
\description{
 The following methods have been implemented: sum, max, min, range
 
 These methods compare layers and the result of these methods is always a single RasterLayer.
 For the extreme values within in a layer use maxValue() and minValue()
}


\author{Robert J. Hijmans }

\examples{
r1 <- raster()
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- 2 * r1 * r1
r3 <- sqrt(r1) * 5
r4 <- max(r1, r2, r3)

}

\keyword{methods}
\keyword{math}
