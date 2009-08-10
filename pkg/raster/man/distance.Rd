\name{distance-methods}
\docType{methods}
\alias{distance-methods}
\alias{distance}

\alias{distance,RasterLayer-method}

\title{Distance}

\description{
 The function calculates the distance to cells of a RasterLayer.
 
 Origin cells should have value TRUE (or 1), destination cells should be \code{NA} and cells that can not be traversed should be set to FALSE (or 0).
 
 The distance is in meters if the RasterLayer is not projected (\code{+proj=longlat})
 and in map units (typically meters) when it is projected.
 
 Distances are calculated by summing local distances between cells, which are connected with their neighbours in 8 directions.
}

\usage{
distance(object, filename = "", filetype = "raster", overwrite = FALSE, datatype = "FLT4S", track=-1) 
}

\arguments{
\item{object}{a RasterLayer object}
\item{filename}{character. output filename}
\item{overwrite}{logical. If \code{TRUE}, "filename" will be overwritten if it exists}
\item{filetype}{character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{rasterFormats}}}
\item{datatype}{character. output data type; see \code{\link[raster]{dataType}}}
\item{track}{vector of row numbers for which the function will report that they have been processed}
}


\seealso{ For more options (directions, cost-distance) see the \code{ \link[gdistance]{gdistance}} package } 


\examples{
r1 <- raster(ncol=36,nrow=18)
r1[] <- NA
r1[345:355] <- 1
distmap <- distance(r1) 
#plot(distmap)
}
\keyword{methods}
\keyword{spatial}
