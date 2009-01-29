\name{distance-methods}
\docType{methods}
\alias{distance-methods}
\alias{distance}
\alias{distance,RasterLayer-method}
\title{ Calculate distance from geographic features in a raster}
\description{
 The function calculates the distance from the non-NA cells of the RasterLayer.
 
 The distance is in meters if the RasterLayer is in a geographic (latlon) projection and in map units when not projected.
 
 Distances are calculated by summing local distances between cells, which are connected with their neighbours in 8 directions.
 
 For more options (directions, cost-distance) see the gdistance package.
}
\section{Methods}{
\describe{

\item{raster = "RasterLayer"}{ ~~describe this method here }
}}
\examples{
r1 <- newRaster(ncol=36,nrow=18)
r1[345:355] <- 1
distmap <- distance(r1) 
#plot(distmap)
}
\keyword{methods}
\keyword{spatial}
