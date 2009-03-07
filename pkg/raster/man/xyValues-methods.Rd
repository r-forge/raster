\name{xyValues}
\docType{methods}

\alias{xyValues}
\alias{xyValues,RasterLayer,matrix-method}
\alias{xyValues,RasterStack,matrix-method}
\alias{xyValues,RasterLayer,SpatialPoints-method}
\alias{xyValues,RasterStack,SpatialPoints-method}

\alias{bilinearValue}
\alias{bilinearValue,RasterLayer,matrix-method}


\title{ values at xy coordinates }

\description{
 These methods return values of a RasterLayer or RasterStack for the cells that correspond to the provided xy coordinates (n*2 matrix or SpatialPoints object)
}

\usage{xyValues(raster, xyCoords)}

\arguments{
\item{raster}{RasterLayer or RasterStack object}
\item{xyCoords}{xy coordinates (see below, under Methods}
}

\section{Methods}{
\describe{
\code{xyCoords} can be a  \code{n*2} matrix or dataframe with the first column having the x values, and the second column has the y values.
It can also be a SpatialPoints* object.


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
}

\keyword{methods}
\keyword{spatial}
