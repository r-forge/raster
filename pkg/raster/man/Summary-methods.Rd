\name{Summary}
\docType{methods}
\alias{Summary-methods}
\alias{Summary,RasterLayer-method}
\alias{Summary,RasterStack-method}
\alias{mean,Raster-method}
\alias{mean,RasterStack-method}

\title{ Summary methods }

\description{
 The following summary methods are available for Raster* objects:
 \code{mean, max, min, range, prod, sum, any, all}
}



\note{
 These methods compare layers and the result of these methods is always a single RasterLayer. 
 The generic functin \code{range} returns 2 values (the minimum and maximum value of a vector). The Raster* implementations returns a single values (the range)
 
 For the extreme values within in a layer use maxValue() and minValue()
}

\value{a RasterLayer}

\author{Robert J. Hijmans }

\examples{
r1 <- raster(nrow=10, ncol=10)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
r3 <- setValues(r1, runif(ncell(r1)))
r <- max(r1, r2, r3)
r <- range(r1, r2, r3)

s <- stack(r1, r2, r3)
r <- mean(s)
}

\keyword{methods}
\keyword{spatial}
