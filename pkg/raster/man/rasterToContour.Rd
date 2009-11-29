\name{rasterToContour}

\alias{rasterToContour}

\title{ Raster to contour lines conversion}

\description{
RasterLayer to contour lines. This is a wrapper around \code{\link[grDevices]{contourLines}}
}

\usage{
rasterToContour(x, maxdim=500, ...)
}

\arguments{
 \item{x}{ a RasterLayer object }
 \item{maxdim}{ Maximum number of raster cells to use (either vertical or horizontal); this function fails when too many cells are used}
 \item{...}{Any argument that can be passed to \code{\link[grDevices]{contourLines}} }  
}

\details{
Most of the code was taken from maptools::ContourLines2SLDF, by Roger Bivand & Edzer Pebesma 
}

\value{
An object of class SpatialLinesDataFrame 
}

\author{Robert J. Hijmans}


\keyword{ spatial }
