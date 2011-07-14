\name{as.raster}

\alias{as.raster}
\alias{as.raster,RasterLayer-method}
  
\title{Coerce to a 'raster' object}

\description{
'raster' objects can be used for plotting with rasterImage. 
}

\usage{
as.raster(x, ...)
}

\arguments{
 \item{x}{ RasterLayer object }
 \item{...}{ Additional arguments. 
 
   \code{maxpixels} Integer. To regularly subsample very large objects 
	
   \code{col} Color function. Default is col=rev(terrain.colors(255)))
	}
}
 

\value{
'raster' object
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=3, nrow=3)
r[] = 1:ncell(r)
as.raster(r)
}

\keyword{spatial}
\keyword{methods}
