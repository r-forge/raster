\name{polygonValues}
\docType{methods}

\alias{polygonValues}
\alias{polygonValues,SpatialPolygons,Raster-method}

\title{ Extract values from a RasterLayer by polygons }

\description{
This method returns the values of a Raster object for the area covered by one or more polygons.
}

\usage{
polygonValues(p, r, ...)
}

\arguments{
\item{p}{SpatialPolygons* object}
\item{r}{Raster object}
\item{...}{additional arguments (none implemented)}
}


\value{
a list as long as the number of polygons in p
}

\seealso{ \code{\link[raster]{cellValues}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{read}}, \code{\link[sp]{SpatialPolygons}}  }

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=18, ncol=9)
r[] <- 1:ncell(r)

cds1 <- rbind(c(-180,-20), c(-160,55), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(-80,0), c(-20,60), c(0,5), c(-60,-45), c(-80,0))
cds3 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2), Polygons(list(Polygon(hole)), 3), Polygons(list(Polygon(cds3)), 4) ))
polys@polygons[[3]]@Polygons[[1]]@hole <- TRUE

#plot(r)
#plot(polys, add=TRUE)
polygonValues(polys, r)
}

\keyword{methods}
\keyword{spatial}
