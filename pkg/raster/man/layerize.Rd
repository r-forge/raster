\name{layerize}

\docType{methods}

\alias{layerize}
\alias{layerize,RasterLayer,missing-method}
\alias{layerize,RasterLayer,RasterLayer-method}

\title{Layerize}

\description{
Create a RasterBrick with a Boolean layer for each value in a RasterLayer (or subset thereof). For example, if cells in a RasterLayer indicate what class they belong to (e.g. vegetation type), this function will create a layer (presence/absence) for each class. You can supply another RasterLayer to layerize the values to a different raster (e.g. higher resolution).
}

\usage{
\S4method{layerize}{RasterLayer,missing}(x, classes=NULL, digits=0, falseNA=FALSE, filename='', ...)

\S4method{layerize}{RasterLayer,RasterLayer}(x, y, classes=NULL, digits=0, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{y}{RasterLayer or missing}
  \item{classes}{numeric. The values for which layers should be made. If \code{NULL} all classes are used}
  \item{digits}{non-negative integer for rounding the cell values. Argument is passed to \code{\link[base]{round}} }
  \item{falseNA}{logical. If \code{TRUE} the list will be merged in a single data.frame}
  \item{filename}{character. Output filename (optional)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}
}

\value{
RasterBrick
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- round(runif(ncell(r))*5)
r[1:5] <- NA
b <- layerize(r)
}

\keyword{spatial}
