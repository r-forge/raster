\name{raster to points}

\alias{rasterToPoints}

\title{ Raster to points conversion}
\description{
Raster to point conversion.
}
\usage{
rasterToPoints(raster, rm.na=TRUE, fun=NULL, asSpatialPoints=FALSE)
}

\arguments{
  \item{raster}{ a RasterLayer object }
  \item{rm.na}{ if \code{TRUE}, cells with NA values will not be converted  }
  \item{fun}{ function to select a subset of raster values}
  \item{asSpatialPoints}{if \code{TRUE}, the function returns a SpatialPointsDataFrame object }
}

\details{
fun should be a simple function returning a logical value
e.g.:
 fun=function(x){x==1}
 fun=function(x){x>3}
}

\author{Robert J. Hijmans}


\examples{ 
 r <- raster(nrow=18, ncol=36)
 r[] <- round(10 * runif(ncell(r)))
 r[r>8] <- NA
 p <- rasterToPoints(r, fun=function(x){x>5})
#plot(r)
#points(p)
}

\keyword{ spatial }
