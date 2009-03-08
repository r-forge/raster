\name{xyValues}
\docType{methods}

\alias{xyValues}
\alias{xyValues,RasterLayer,matrix-method}
\alias{xyValues,RasterStack,matrix-method}
\alias{xyValues,RasterLayer,SpatialPoints-method}
\alias{xyValues,RasterStack,SpatialPoints-method}

\title{ values at xy coordinates }

\description{
 These methods return values of a RasterLayer or RasterStack for the cells that correspond to the provided xy coordinates (n*2 matrix or SpatialPoints object)
}

\usage{
xyValues(object, xyCoords, ...)
}

\arguments{
\item{object}{RasterLayer or RasterStack object}
\item{xyCoords}{xy coordinates (see below, under Methods}
\item{...}{additional arguments. See below}

}

\section{Methods}{
\describe{
\code{xyCoords} can be a  \code{n*2} matrix or dataframe with the first column having the x values, and the second column has the y values.
It can also be a \code{SpatialPoints*} object.

if \code{raster} is a \code{RasterLayer} object, the additional argument \code{method='bilinear'} can be used. In the case, the returned values
are interpolated from the (centers of the) four nearest raster cells.

}
}


\value{
a vector of cell values for a RasterLayer or or a matrix of values for a RasterStack
}

\seealso{ \code{\link[raster]{cellValues}}, \code{\link[raster]{readRow}}, \code{\link[raster]{readAll}}, \code{\link[sp]{SpatialPoints}}  }

\author{Robert J. Hijmans }

\examples{
#using a new default raster (1 degree global)
r <- raster()
r[] <- 1:ncell(r)
xy <- cbind(0:36 * 10 - 180, 90)
xyValues(r, xy)
xyValues(r, xy, method='bilinear')
}

\keyword{methods}
\keyword{spatial}
