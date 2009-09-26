\name{xyValues}
\docType{methods}

\alias{xyValues}
\alias{xyValues,Raster,vector-method}
\alias{xyValues,Raster,SpatialPoints-method}
\alias{xyValues,Raster,data.frame-method}

\alias{xyValues,RasterLayer,matrix-method}
\alias{xyValues,RasterStack,matrix-method}
\alias{xyValues,RasterBrick,matrix-method}

\title{ Extract values at xy coordinates }

\description{
These methods return the values of a RasterLayer or RasterStack for a set of xy coordinates (points).
The xy coordinates can be passed as a vector of length 2; a n*2 matrix or data.frame; or a SpatialPoints object.

Values can be extracted using bilnear interpolation of values of the four cells nearest to a point.
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
A full call: 

\code{xyValues(object, xyCoords, method='simple')}

\tabular{rll}{
\tab \code{object} \tab a Raster* object \cr
\tab \code{xyCoords} \tab a  \code{n*2} matrix or dataframe with the first column having the x values, and the second column has the y values.
It can also be a \code{SpatialPoints*} object \cr
\tab \code{method} \tab character. \code{'simple'} or \code{'bilinear'}. In the case of \code{'bilinear'}, the returned values
are interpolated from the (centers of the) four nearest raster cells. \cr
}

}
}

\value{
a vector (object is a RasterLayer) or a matrix (object is a RasterStack)
}

\seealso{ \code{\link[raster]{cellValues}}, \code{\link[raster]{read}}, \code{\link[sp]{SpatialPoints}}  }

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
