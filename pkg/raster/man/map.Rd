\name{map}
\alias{map}
\alias{plot}
\alias{plot,RasterLayer,missing-method}
\alias{plot,RasterLayer,RasterLayer-method}
\alias{plot,RasterStack,numeric-method}
\alias{plot,RasterStack,missing-method}
\alias{click}

\title{ Map a raster layer }
\description{
  Make a map of a RasterLayer
}
\usage{
map(object, index=1, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...)
click(object, n=1, xy=FALSE, type = "n", ...)
}

\arguments{
  \item{object}{RasterLayer object}
  \item{index}{Ignored for RasterLayers. The index of a raster in a RasterStack  1 <= index <= nlayers(rstack) }
  \item{col}{A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, on one or your own making}
  \item{subsample}{Logical. If \code{TRUE}, the image is resampled using maxdim}
  \item{maxdim}{Maximum requested number of pixels in the map display (either vertical or horizontal) }
  \item{addbox}{Logical. If \code{TRUE} a box is drawn around the map } 
  \item{axes}{see "?image"}
  \item{xlab}{see "?image"}
  \item{ylab}{see "?image"}
  \item{...}{For plot/map: Any argument that can be passed to image.plot(fields) and to image(graphics)} 
  
  \item{n}{number of clicks on the map}
  \item{xy}{show xy coordinates}
  \item{type}{see under ?locator}
 }
\details{
  If \code{subsample=FALSE}, then \code{maxdim} is ingored. Subsampled maps are made by skipping rows and columns. This is always 
  done in a regular pattern (e.g. skip every second row). For this reason, the actual number of pixels when \code{subsample = TRUE}
  is generally not exactely the same as \code{maxdim}. If \code{maxdim} is close to the largest dimension of the raster data matrix 
  it does not affect it. It needs to be 0.75 times the largest raster dimension to have an effect, in which case the number of pixels
  will be 0.5 times the largest dimension times the smallest dimension.  
  
  Most of this function was taken from image.plot (fields package)
  map is also called when plot() is used with a Raster* object as first argument
}


\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{ 
	rs <- raster()
	rs <- setValues(rs, runif(ncell(rs)))
	map(rs)
	rsa <- Aggregate(rs, fact=10, fun=max)
	map(rsa)
 }

\keyword{ spatial }
