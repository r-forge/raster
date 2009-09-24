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
distance(object, filename = "", ...) 
}

\arguments{
\item{object}{a RasterLayer object}
\item{filename}{character. output filename}
\item{...}{additional arguments. See Details.}  
}


\seealso{ For more options (directions, cost-distance) see the \code{ \link[gdistance]{gdistance}} package } 

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{filetype} \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\examples{
r1 <- raster(ncol=36,nrow=18)
r1[] <- NA
r1[345:355] <- 1
distmap <- distance(r1) 
#plot(distmap)
}
\keyword{methods}
\keyword{spatial}
