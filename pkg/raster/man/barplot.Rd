\name{barplot}

\docType{methods}

\alias{barplot}
\alias{barplot,RasterLayer-method}

\title{barplot}

\description{
Create a barplot of the values of a RasterLayer. For large datasets a sample is used.
}

\usage{
\S4method{barplot}{RasterLayer}(height, maxpixels=1000000, digits=0, cuts=NULL, col=rainbow, ...)
}

\arguments{
  \item{height}{RasterLayer}
  \item{maxpixels}{integer. To regularly subsample very large objects}
  \item{digits}{integer. Used in \code{\link{round}}. Ignored when \code{cuts} is not \code{NULL}}
  \item{cuts}{integer. Number of \code{\link{cut}}s used to group the data}
  \item{col}{color generitic function or vector of colors}
  \item{...}{Additional arguments for plotting}
}


\seealso{ \code{\link{hist}, \link{boxplot}} }

\examples{ 
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
barplot(r, cuts=10)
barplot(r, digits=-2)
}

\keyword{methods}
\keyword{spatial}

