\name{zoom}

\alias{zoom}

\title{Zoom in on a plot}

\description{
  Zoom in on a plot (map) by providing a new extent, by default this is done by clicking on the map 
}

\usage{
zoom(x, extent=drawExtent(), maxpixels=100000, new=TRUE, ...) 
}

\arguments{
\item{x}{ a RasterStack or RasterBrick object }
\item{extent}{Extent object}  
\item{maxpixels}{Maximum number of pixels used for the map}  
\item{new}{Logical. If \code{TRUE}, the zoomed in map will appear on a new device (window) }  
\item{...}{Raster* objects or filenames (character) }
}


\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{drawExtent}}, \code{\link[raster]{plot}}}

\keyword{ spatial }

