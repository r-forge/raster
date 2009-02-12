\name{polygons to raster}

\alias{polygonsToRaster}

\title{ Polygons to raster conversion}
\description{
Two algorithms for polygon to raster conversion. The polygons are rasterized. Either values associated with each polygon, or a polygon ID is transferred. Holes in polygons are recognized by polygonsToRaster, but not by polygonsToRaster2.
}
\usage{
polygonsToRaster(spPolys, raster, field=0, filename="", overwrite=FALSE, updateRaster=FALSE, updateValue="NA",  filetype='raster',  datatype='FLT4S', track=c(100, 500, 1:(round(nrow(raster)/1000)) * 1000)) 
}

\arguments{
  \item{spPolys}{ a SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{ RasterLayer  }
  \item{field}{ The index of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer }
  \item{filename}{ output filename }
  \item{overwrite}{ logical. if \code{TRUE} ouput file will be overwritten if it exists }
  \item{updateRaster}{logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{character. Select cells to be updated (if \code{updateRaster == TRUE}) by their current values. Either 'all', 'NA', '!NA', or 'zero' } 
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed. To track progress on very large files }
}

\details{
For SpatialPolygons, the 'field' argument is ignored. The polygon index is tranferred.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{linesToRaster}}, \code{\link[raster]{pointsToRaster}}}

\examples{ 
cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25), c(-50,0))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55), c(80,20))
cds3 <- rbind(c(-180,-20), c(-140,55), c(-60,-20), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))

polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2), Polygons(list(Polygon(cds3)), 3), Polygons(list(Polygon(hole)), 4) ))
polys@polygons[[4]]@Polygons[[1]]@hole <- TRUE
r <- raster()
r <- polygonsToRaster(polys, r)
#plot(r)
#plot(polys, add=T)
cds4 <- rbind(c(-180,10), c(0,40), c(40,45), c(145,-10),  c(-25, -15), c(-180,0), c(-180,10))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(cds4)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- polygonsToRaster(addpoly, r, field=1, updateRaster=TRUE, updateValue="NA")
#plot(r2)
#plot(polys, border="blue", lwd=2, add=TRUE)
#plot(addpoly, add=TRUE, border="red", lwd=2)
}

\keyword{ spatial }
