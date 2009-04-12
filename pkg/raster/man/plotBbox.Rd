\name{plotBbox}

\docType{methods}

\alias{plot,BoundingBox,ANY-method}
\alias{plot,BoundingBox,missing-method}


\title{Methods for Function plot in Package `raster'}

\description{
 Methods for function \code{plot} in Package `raster'
 
 \code{plot} can also be supplied with the two RasterLayer objects, creating a scatter plot (if they have matching extent and resolution)

}

\section{Methods}{
\describe{

Methods for function \code{plot}

\item{x = "Raster", y = "missing"}{creates a plot of raster values; see \code{\link[raster]{map}}  }

\item{x = "RasterLayer", y = "RasterLayer"}{ xy plot of RasterLayer values }

\item{x = "BoundingBox", y = "ANY"}{ plot a BoundingBox object }

}}


\seealso{ \code{\link[raster]{plot}} }


\keyword{methods}
\keyword{spatial}

