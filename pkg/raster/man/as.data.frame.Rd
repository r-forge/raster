\name{as.data.frame}

\alias{as.data.frame}
\alias{as.data.frame,Raster-method}
\alias{as.data.frame,SpatialPolygons-method}
\alias{as.data.frame,SpatialLines-method}
\alias{as.data.frame,SpatialPoints-method}
  

\title{Get a data.frame with raster cell values, or coerce a SpatialPolygons, Lines, or Points file to a data.frame}

\description{
\code{as.matrix} returns all values of a Raster* object as a matrix. For RasterLayers, rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects, the matrix returned by \code{as.matrix} has columns for each layer and rows for each cell. 

\code{as.array} returns an array of matrices that are like those returned by \code{as.matrix} for a RasterLayer

If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file.

}

\usage{
\S4method{as.data.frame}{Raster}(x, row.names=NULL, optional=FALSE, ...)

\S4method{as.data.frame}{SpatialPolygons}(x, row.names=NULL, optional=FALSE, xy=FALSE, centroids=TRUE, ...)

\S4method{as.data.frame}{SpatialLines}(x, row.names=NULL, optional=FALSE, xy=FALSE, ...)

\S4method{as.data.frame}{SpatialPoints}(x, row.names=NULL, optional=FALSE, xy=FALSE, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row.names}{NULL or a character vector giving the row names for the data frame. Missing values are not allowed} 
 \item{optional}{logical. If TRUE, setting row names and converting column names (to syntactic names: see make.names) is optional}
 \item{xy}{logical. If \code{TRUE}, also return the spatial coordinates}
 \item{centroid}{logical. If \code{TRUE} return the centroid instead of all spatial coordinates}
 \item{...}{Additional arguments (none)} 
}
  

\value{
data.frame
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=3, nrow=3)
r[] = 1:ncell(r)
as.data.frame(r)
s <- stack(r,r)
as.data.frame(s)
}

\keyword{spatial}
\keyword{methods}
