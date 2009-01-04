\name{RasterBrick-class}
\docType{class}
\alias{RasterBrick-class}
\alias{show,RasterBrick-method}
\alias{plot,RasterBrick,numeric-method}

\title{Class "RasterBrick" }
\description{	Class for handling "RasterBrick" objects  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterBrick", ...)}.
}

\section{Slots}{
 \describe{
    \item{\code{filename}:}{Object of class \code{"character"} filename of the RasterBrick (if saved to disk) }
    \item{\code{nlayers}:}{Object of class \code{"integer"} number of layers in the RasterBrick }
    \item{\code{projection}:}{Object of class \code{"character"} proj4 projection string }
    \item{\code{ncols}:}{Object of class \code{"integer"} number of columns of the rasters }
    \item{\code{nrows}:}{Object of class \code{"integer"} number of rows of the rasters }
    \item{\code{ncells}:}{Object of class \code{"integer"} number of cells of the rasters }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RasterBrick")}: ... }
	 }
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[RasterLayer]{RasterLayer}} }

\examples{
showClass("RasterBrick")
}
\keyword{classes}
\keyword{ spatial }
