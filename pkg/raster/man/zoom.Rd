\name{zoom}

\alias{zoom}

\title{Zoom in on a plot}

\description{
  Zoom in on a plot (map) by providing a new extent, by default this is done by clicking on the map 
}

\usage{
zoom(x, extent=drawBox(), new=TRUE, ...) 
}

\arguments{
  \item{x}{ a RasterStack or RasterBrick object }
  \item{extent}{Extent object}  
  \item{new}{Logical. If \code{TRUE}, the zoomed in map will appear in a new window }  
  \item{...}{Raster* objects or filenames (character) }
}


\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{drawBox}}, \code{\link[raster]{plot}}}

\keyword{ spatial }
