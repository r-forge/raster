\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,numeric,RasterLayer-method}
\alias{Arith,RasterLayer,numeric-method}
\alias{Arith,RasterLayer,RasterLayer-method}
\title{ Methods for arithmic function for RasterLayer objects }
\description{
  The following standard arithmic functions can be used for computations with RasterLayers objects and numeric values
  "+", "-", "*", "^", "%%", "%/%", "/" 
}
\section{Note}{
 The RasterLayer objects must have either all data in memory or on disk. If the data do not fit into memory you should use the Overlay() function instead.
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
r1 <- newRaster()
r1 <- setValues(r1, runif(ncells(r1)))
r2 <- setValues(r1, 1:ncells(r1)) / ncells(r1)
r3 <- r1 * r2 - 1
}

\keyword{methods}
\keyword{math}
