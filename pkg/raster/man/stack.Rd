\name{stack}

\docType{methods}

\alias{stack}

\alias{stack,character-method}
\alias{stack,Raster-method}
\alias{stack,list-method}
\alias{stack,missing-method}
\alias{stack,SpatialPixelsDataFrame-method}
\alias{stack,SpatialGridDataFrame-method}

\title{ Create a RasterStack object}

\description{
A RasterStack is a collection of RasterLayer objects with the same spatial extent and resolution. 
A RasterStack can be created from RasterLayer objects, or from raster files, or both. The can also be created from SpatialPixels or SpatialGrid objects.
}

\section{Methods}{
\describe{
\code{stack(x, ..., bands=NULL)}

\item{\code{x}}{ filename (character), Raster* object, SpatialGrid*, SpatialPixels*, or list (of filenames and/or Raster* objects) }
\item{\code{...}}{additional RasterLayer objects (or filenames) }
\item{\code{bands}}{Which bands (layers) of the file should be used (default is all layers)}

\code{x} can be a filename from which a RasterLayer can be created (with \code{raster}, a Raster* object, 
or a list of filenames and/or Raster* objects.

If \code{x} is a \code{list}, additional arguments  \code{...} are ignored.
}}

\value{
RasterStack
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{addLayer}}, \code{\link[raster]{dropLayer}}, \code{\link[raster]{raster}} }

\examples{
fn <- system.file("external/test.grd", package="raster")
s <- stack(fn, fn)
r <- raster(fn)
s <- stack(r, fn) 
}

\keyword{methods}
\keyword{spatial}
