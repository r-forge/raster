\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,numeric,RasterLayer-method}
\alias{Arith,RasterLayer,numeric-method}
\alias{Arith,RasterLayer,RasterLayer-method}
\title{Methods for arithmic function for RasterLayer objects}
\description{
  Standard arithmic functions for computations with RasterLayer objects and numeric values 
}
\section{Note}{
 The RasterLayer objects must have either all data in memory or on disk. If the data do not fit into memory you should use the overlay() function instead.
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
r1 <- raster(ncols=10, nrows=10)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, 1:ncell(r1)) / ncell(r1)
r3 <- r1 * (r2 - 1 + r1^2 / r2)
}

\keyword{methods}
\keyword{math}
