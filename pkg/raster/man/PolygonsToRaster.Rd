\name{polygons to raster}

\alias{polygonsToRaster}
\alias{polygonsToRaster2}

\title{ Transform polygons to a raster }
\description{
  Two implementations of a polygon to raster conversion (number 2 should be slowest in most cases)
}
\usage{
polygonsToRaster(sppoly, raster, field=0, filename="", overwrite=FALSE, updateRaster=FALSE, updateValue="NA") 
polygonsToRaster2(sppoly, raster, field=0, filename="", overwrite=FALSE) 
}

\arguments{
  \item{sppoly}{ a SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{ RasterLayer  }
  \item{field}{ The index of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer }
  \item{filename}{  }
  \item{overwrite}{  }
  \item{updateRaster}{logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{character. Select cells to be updated (if \code{updateRaster == TRUE}) by their current values. Either 'all', 'NA', '!NA', or 'zero' } 
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
r <- raster()
r <- polygonsToRaster(polys, r)
#plot(r)
#plot(polys, add=T)
cds4 <- rbind(c(-180,20), c(0,90), c(40,85), c(150, 45), c(180,-5), c(145,-40), c(40,-35), c(0,-25), c(-25, -15), c(-180,20))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(cds4)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- polygonsToRaster(addpoly, r, field=1, updateRaster=TRUE, updateValue="NA")
#plot(r2)
#plot(polys, add=T)
#plot(addpoly, add=T, border="red", lwd=2)

}

\keyword{ spatial }
