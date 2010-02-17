\name{zoom}

\alias{zoom}

\title{Zoom in on a plot}

\description{
Zoom in on a plot (map) by providing a new extent, by default this is done by clicking on the map 
}

\usage{
zoom(x, extent=drawExtent(), maxpixels=100000, layer=1, new=TRUE, ...) 
}

\arguments{
\item{x}{Raster* object }
\item{extent}{Extent object}  
\item{maxpixels}{Maximum number of pixels used for the map}  
\item{layer}{Positive integer to select the layer to be used if x is a mutilayer Raster object}  
\item{new}{Logical. If \code{TRUE}, the zoomed in map will appear on a new device (window) }  
\item{...}{Raster* objects or filenames (character) }
}


\value{
An extent object (invisibly)
}


\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{drawExtent}}, \code{\link[raster]{plot}}}

\keyword{ spatial }

