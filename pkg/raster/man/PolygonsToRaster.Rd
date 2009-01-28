\name{polygons to raster}

\alias{polygonsToRaster}
\alias{polygonsToRaster2}

\title{ Transform polygons to a raster }
\description{
  Two implementations of a polygon to raster conversion (#2 should be slowest in most cases)
}
\usage{
polygonsToRaster(sppoly, raster, field=0, filename="", overwrite=FALSE) 
polygonsToRaster2(sppoly, raster, field=0, filename="", overwrite=FALSE) 
}

\arguments{
  \item{sppoly}{ a SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{ RasterLayer  }
  \item{field}{ The index of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer }
  \item{filename}{  }
  \item{overwrite}{  }
  
}

\details{
	For SpatialPolygons, the 'field' argument is ignored. The polygon index is tranferred.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25), c(-50,0))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55), c(80,20))
cds3 <- rbind(c(-180,-20), c(-140,-60), c(-60,-20), c(-140,55), c(-180,-20))

polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2), Polygons(list(Polygon(cds3)), 3) ))
r <- newRaster()
r <- polygonsToRaster(polys, r)
#plot(r)
#plot(polys, add=T)

}

\keyword{ spatial }
