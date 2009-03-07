\name{Logic-methods}
\docType{methods}
\alias{Logic-methods}
\alias{Logic,RasterLayer,RasterLayer-method}
\alias{is.na,RasterLayer-method}
\alias{is.nan,RasterLayer-method}
\alias{is.finite,RasterLayer-method}
\alias{is.infinite,RasterLayer-method}
\alias{!,RasterLayer-method}


\title{ Methods for logical functions for RasterLayers' }
\description{
The following logical (boolean) operators are available for computations with RasterLayer objects: \code{"&", "|", and "!"}
These functions are also available with a RasterLayer argument: \code{is.na}, \code{is.nan}, \code{is.finite}, \code{is.infinite}
}

\value{
A RasterLayer object with logical (\code{TRUE/FALSE} values)
}


\section{Note}{
 If the values of the output RasterLayer cannot be held in memory, they will be saved to a temporary file. 
 If you want to set the filename (and perhaps filetype and datatype), use \code{\link[raster]{calc-methods}} instead.
}

\seealso{ \code{\link[raster]{Math-methods}}, \code{\link[raster]{overlay}}, \code{\link[raster]{calc}} }


\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- runif(ncell(r)) * 10
r <- r < 3 | r < 6
r <- !r
}

\keyword{methods}
\keyword{math}
