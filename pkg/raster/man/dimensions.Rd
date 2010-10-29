\name{dimensions}

\alias{dim, BasicRaster-method}
\alias{dim, RasterStackBrick-method}
\alias{dim<-,RasterLayer-method}
\alias{dim<-,RasterBrick-method}

\docType{methods}


\title{Dimensions of a Raster* object}

\description{
Get (or set) the number of rows, columns, and layers of a Raster* object. You cannot use this function to set the dimentions of a RasterStack object.
}

\section{Methods}{
\describe{
\code{contour(x, y=1, maxpixels=100000, ...)}

\tabular{rll}{
\tab \code{x} \tab Raster* object (only RasterLayer and RasterBrick for dim<-) \cr
\tab \code{value} \tab row or column number. For dim<- it can be a row _and_ column number (for a RasterLayer and a RasterBrick) or a row and column number _and_ the number of layers (only for a RasterBrick)  \cr
}
}}

\value{
Integer
}

\seealso{ \code{\link{extent}, \link{res}} }

\author{ Robert J. Hijmans }

\examples{
r <- raster()
dim(r)
dim(r) <- c(18, 36) 
dim(r)
}

\keyword{spatial}

