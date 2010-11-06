\name{pointsToRaster}

\alias{pointsToRaster}

\title{Point to raster conversion}

\description{ 
Obsolete, see \code{rasterize}
}

\usage{
pointsToRaster(raster, xy, values=1, fun=NULL, background=NA, na.rm=TRUE, filename="", ...)
}

\arguments{
  \item{raster}{Raster* object}
  \item{xy}{Matrix of x and y coordinates (in the same map reference system as the raster) or a SpatialPoints* object}
  \item{values}{Vector of length of the number of points (i.e., dim(xy)[1] if xy is a matrix) with the values to be used; alternatively 
  you can supply a single value which is then assigned to each point. You can also supply a matrix if its number of rows matches the number of points. If the matrix has more than one column, a RasterBrick will be returned, with a layer for each column in \code{values}. }
  
  \item{fun}{Function to be applied, on a cell by cell basis, to the (values of) the points that fall in a cell. If no function is specified, the number of points per cell is returned. Typical examples of functions that can be used are \code{mean}, \code{sum}, \code{min} and \code{max}. Note that the function must take an \code{na.rm} argument, either explicitly or through 'dots'. This 
  \code{fun=lenght} fails, but \code{fun=function(x,...)lenght(x)} works (but ignores the \code{na.rm} argument. To use the \code{na.rm} argument you can use a function like this \code{fun=function(x,na.rm){if (na.rm) length(na.omit(x)) else (length(x)}}, or use a function that removes NA values in all cases, like this functions for compute the number of unique values "richness": \code{fun=function(x, ...){length(unique(na.omit(x)))}}. 
  You can also pass multiple functions using a statement like \code{fun=function(x)c(lenght(x),mean(x))}, in which case the returned object is a RasterBrick (multiple layers).}
  
  \item{background}{Numeric. The value to be applied to the cells in which no points fall (typically \code{NA} or \code{0})}
  \item{na.rm}{Logical. Remove \code{NA} values? This argument might be ignored by \code{fun} }
  \item{filename}{Character. Output filename}
  \item{...}{Additional arguments. See Details}  
}


\value{
RasterLayer
}

\author{Robert J. Hijmans, Steven Mosher & Paul Hiemstra}

\seealso{ \code{\link{rasterize}}}


\keyword{spatial}
