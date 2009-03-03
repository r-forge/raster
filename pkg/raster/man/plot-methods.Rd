\name{plot}

\docType{methods}

\alias{plot,BoundingBox,ANY-method}
\alias{lines}
\alias{lines,BoundingBox-method}
\alias{plot,BoundingBox,missing-method}
\alias{plot,Raster,missing-method}
\alias{plot,Raster,numeric-method}
\alias{plot,RasterLayer,RasterLayer-method}

\title{Methods for Function plot in Package `raster'}

\description{
 Methods for function \code{plot} in Package `raster'
 
 \code{plot} can also be supplied with an additional numeric argument indicating the layer in a RasterStack object that should be plotted (mapped)
 \code{plot} can also be supplied with the two RasterLayer objects, creating a scatter plot (if they have matching extent and resolution)

}

\section{Methods}{
\describe{


\item{x = "Raster", y = "missing"}{creates a plot of raster values; see \code{\link[raster]{map}}  }

\item{x = "Raster", y = "numeric"}{ creates a plot (map) of raster values. The second argument is the layer to be shown for multlayer Raster* objects  }

\item{x = "RasterLayer", y = "RasterLayer"}{ xy plot of RasterLayer values }

\item{x = "BoundingBox", y = "ANY"}{ plot a BoundingBox object }

}}
\keyword{methods}
\keyword{spatial}

