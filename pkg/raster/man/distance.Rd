\name{distance}

\alias{distance}
\alias{distanceFromPoints}


\title{Distance}

\description{
The function calculates the distance to cells of a RasterLayer. Function \code{distance} computes the distance for all cells that are \code{NA} to the nearest cell that is not \code{NA}.
function \code{distanceFromPoints} computes the distance for all cells to the nearest point in a set of points.
 
The distance unit is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically meters) when it is projected.
}

\usage{
distance(object, filename='', ...) 
distanceFromPoints(object, xy, filename='', ...) 
}

\arguments{
\item{object}{a RasterLayer object}
\item{filename}{Output filename (can be absent for RasterLayers that can be stored in memory)}
\item{xy}{matrix of x and y coordinates, or a SpatialPoints* object}
\item{...}{additional arguments. See Details.}  
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\author{Robert J. Hijmans }

\value{A RasterLayer object}

\examples{
r1 <- raster(ncol=36,nrow=18)
r1[] <- NA
r1[345:355] <- 1
dist <- distance(r1) 
#plot(dist)
}

\keyword{spatial}
