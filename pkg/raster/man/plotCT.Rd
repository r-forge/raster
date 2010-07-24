\name{plotCT}

\docType{methods}

\alias{plotCT}
\alias{plotCT,RasterLayer-method}

\title{Color-table plot of a single RasterLayer}


\description{
Make a plot from a single RasterLayer and a color table. If no color-table is supplied, an attempt is made to read one from disk via rgdal (e.g. read it from a geotiff file). 
}

\section{Methods}{
\describe{

\code{plotCT(x, coltab=NULL, maxpixels=100000, extent=NULL, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{coltab}  \tab Color table. A vector of RGB values whose indices match the values of the RasterLayer  \cr 
\tab \code{scale}  \tab Integer. Maximum (possible) value in the three channels \cr 
\tab \code{maxpixels}  \tab  Maximum number of pixels to use  \cr
\tab \code{extent}  \tab  An extent object to zoom in a region \cr
\tab \code{...} \tab Any argument that can be passed to \code{\link[graphics]{image}} (graphics package) \cr
}
}}

\author{ Robert J. Hijmans; based on functions in the pixmap package by Friedrich Leisch }

\seealso{ \code{\link[raster]{plotRGB}} }


\keyword{methods}
\keyword{spatial}

