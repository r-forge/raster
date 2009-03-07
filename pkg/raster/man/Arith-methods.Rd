\name{Arith-methods}

\docType{methods}

\alias{Arith-methods}
\alias{Arith,numeric,RasterLayer-method}
\alias{Arith,RasterLayer,numeric-method}
\alias{Arith,RasterLayer,RasterLayer-method}

\title{arithmic functions}

\description{
  Standard arithmic functions, \code{"+", "-", "*", "/", "^", "\%\%", "\%/\%"}, for computations with RasterLayer objects and numeric values.
  Functions are applied on a cell by cell basis (single numeric values are recycled). 
  Input RasterLayers must have the same extent and resolution.
}

\section{Note}{
 If the values of the output RasterLayer cannot be held in memory, they will be saved to a temporary file. 
 If you want to set the filename (and perhaps filetype and datatype), use \code{\link[raster]{overlay-methods}} or \code{\link[raster]{calc-methods}} instead of the Arith-methods.
}

\seealso{ \code{\link[raster]{Math-methods}}, \code{\link[raster]{overlay-methods}}, \code{\link[raster]{calc-methods}} }


\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=10, nrows=10)
r1[] <- runif(ncell(r1))
r2 <- setValues(r1, 1:ncell(r1) / ncell(r1) )
r3 <- r1 + r2
r2 <- r1 / 10
r3 <- r1 * (r2 - 1 + r1^2 / r2)
}

\keyword{methods}
\keyword{math}
