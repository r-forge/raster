\name{polygonsToRaster}

\alias{polygonsToRaster}

\title{Polygons to raster conversion}

\description{
Polygon to raster conversion. The polygons are rasterized. Either values associated with each polygon, or a polygon ID is transferred. 
Holes in polygons are recognized if they are correctly specified.
}

\usage{
polygonsToRaster(spPolys, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", filename="", silent=FALSE, ...) 
}

\arguments{
  \item{spPolys}{a SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{a RasterLayer}
  \item{field}{The index (or colname) of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer (see Details)}
  \item{overlap}{character. Determines what to do with cells with overlapping polygons. Either 'first', 'last', 'sum', 'min', or 'max'}
  \item{filename}{output filename }
  \item{mask}{logical. If \code{TRUE} the values of the input RasterLayer are 'masked' by the polygons. I.e. cells overlapping with the polygons retain their values, the other cells become NA }
  \item{updateRaster}{logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{character. Select cells to be updated (if \code{updateRaster == TRUE}) by their current values. Either \code{'all'}, \code{'NA'}, \code{'!NA'}, or \code{'zero'} } 
  \item{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed}    
  \item{...}{additional arguments. See Details.}    
}

\details{
For SpatialPolygons, the \code{field} argument is ignored. The polygon index is used (i.e. numbers from 1 to the number of polygons. This is also 
done for SpatialPolygonDataFrame objects when \code{field}==0. If  \code{field} < 0, all polygons get the value 1.

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{linesToRaster}}, \code{\link[raster]{pointsToRaster}}}

\examples{ 

cds1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
cds2 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25), c(-50,0))
cds3 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))

polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2), Polygons(list(Polygon(cds3)), 3), Polygons(list(Polygon(hole)), 4) ))
polys@polygons[[4]]@Polygons[[1]]@hole <- TRUE

r <- raster()
r <- polygonsToRaster(polys, r, overlap='sum')
#plot(r)
#plot(polys, add=T)
cds4 <- rbind(c(-180,10), c(0,90), c(40,90), c(145,-10),  c(-25, -15), c(-180,0), c(-180,10))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(cds4)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- polygonsToRaster(addpoly, r, field=1, updateRaster=TRUE, updateValue="NA")
#plot(r2)
#plot(polys, border="blue", lwd=2, add=TRUE)
#plot(addpoly, add=TRUE, border="red", lwd=2)
}

\keyword{ spatial }

