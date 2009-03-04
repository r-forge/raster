\name{Summary}
\docType{methods}

\alias{Summary,Raster-method}
\alias{mean,Raster-method}
\alias{median}
\alias{median,ANY-method}
\alias{median,Raster-method}

\title{ Summary methods }

\description{
 The following summary methods are available for Raster* objects:
 
 \code{mean, median, max, min, range, prod, sum, any, all}
}

\note{
 These methods compare layers and the result of these methods is always a single RasterLayer. 
 
 You can mix RasterLayer, RasterStack objects and single numeric or logical values. 
 However, because generic functions are used, the appropriate method is chosen based on the first argument: '\code{x}'. 
 This means that if \code{r} is a RasterLayer object, \code{mean(r, 5)} will work, but \code{mean(5, r)} will not work.
 
 The generic function \code{range} returns 2 values (the minimum and maximum value of a vector). The Raster* implementations returns a single values (the range)
 
 For the extreme values within in a layer use \code{maxValue} and \code{minValue}
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

