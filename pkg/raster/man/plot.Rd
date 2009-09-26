\name{plot}

\docType{methods}

\alias{plot}
\alias{plot,RasterLayer,missing-method}
\alias{plot,RasterStackBrick,ANY-method}
\alias{plot,RasterLayer,RasterLayer-method}
\alias{plot,Extent,ANY-method}
\alias{plot,Extent,missing-method}

\title{Plot a Raster* object}

\description{
Plot (that is, make a map of) the values of a RasterLayer or RasterStack, or make a scatterplot of their values.
}



\section{Methods}{
\describe{
\bold{x=RasterLayer}

\code{plot(x, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{col}  \tab  A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, on one or your own making \cr
\tab \code{subsample}  \tab  Logical. If \code{TRUE}, the image is resampled using maxdim \cr
\tab \code{maxdim}  \tab  Maximum requested number of pixels in the map display (either vertical or horizontal)  \cr
\tab \code{addbox}  \tab  Logical. If \code{TRUE} a box is drawn around the map \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}  \cr
}



\bold{x=RasterStack or RasterBrick, y=numeric or missing}

\code{plot(x, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterStack or RasterBrick object \cr
\tab \code{y} \tab The index of a RasterLayer in a RasterStack to be plotted. Either a single number between 1 and nlayers(rstack), a vector of numbers, or missing. If y is missing, all RasterLayers in the RasterStack will be plotted.\cr
\tab \code{col}  \tab  A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, on one or your own making \cr
\tab \code{subsample}  \tab  Logical. If \code{TRUE}, the image is resampled using maxdim \cr
\tab \code{maxdim}  \tab  Maximum requested number of pixels in the map display (either vertical or horizontal)  \cr
\tab \code{addbox}  \tab  Logical. If \code{TRUE} a box is drawn around the map \cr
\tab \code{axes}  \tab  see \code{?image} \cr
\tab \code{xlab}  \tab  see \code{?image} \cr
\tab \code{ylab}  \tab  see \code{?image} \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}  \cr
}



\bold{x=RasterLayer, y=RasterLayer}
Produces a scatter plot of the values of x against those of y. 
\code{plot(x, y, maxdim=10000, cex=0.1, ...) }

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{y} \tab a RasterLayer object \cr
\tab \code{maxdim}  \tab  Maximum requested number of pixels in the map display (either vertical or horizontal)  \cr
\tab \code{cex}  \tab  point size \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[graphics]{plot}}  \cr
}

\bold{x=Extent, y="ANY"}
Draws the (rectangular) bounding box of a raster object. 
\code{plot(ext) }

\tabular{rll}{
\tab \code{ext} \tab a Extent object \cr
}

}}


\details{
If \code{subsample=FALSE}, then \code{maxdim} is ingored. Subsampled maps are made by skipping rows and columns. This is always 
done in a regular pattern (e.g. skip every second row). For this reason, the actual number of pixels when \code{subsample = TRUE}
is generally not exactely the same as \code{maxdim}. If \code{maxdim} is close to the largest dimension of the raster data matrix 
it does not affect it. It needs to be 0.75 times the largest raster dimension to have an effect, in which case the number of pixels
will be 0.5 times the largest dimension times the smallest dimension.  
  
Most of the code for this function was taken from image.plot (fields package)
}


\author{ Robert J. Hijmans }

\examples{ 
r <- raster(nrows=36, ncols=18)
r <- setValues(r, runif(ncell(r)))
plot(r)
plot(extent(r))
}

\keyword{methods}
\keyword{spatial}

