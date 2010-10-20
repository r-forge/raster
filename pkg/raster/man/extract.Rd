\name{extract}

\docType{methods}

\alias{extract}
\alias{extract,Raster,vector-method}
\alias{extract,Raster,matrix-method}
\alias{extract,Raster,data.frame-method}
\alias{extract,Raster,SpatialPoints-method}
\alias{extract,Raster,SpatialLines-method}
\alias{extract,Raster,SpatialPolygons-method}
\alias{extract,Raster,Extent-method}

\title{Extract values from Raster object using other spatial data}

\description{
Extract data from a Raster object at the locations of other spatial data (i.e. a spatial query). This _generic_ function will replace a number of existing functions in this package (cellValues, xyValues, lineValues, and polygonValues)
}

\usage{
extract(x, y, ...) 
}


\arguments{
  \item{x}{Raster* object}
  \item{y}{A vector (representing cell numbers) or a two-column data.frame or matrix (representing points), or a SpatialPoints, SpatialLines, SpatialPolygons, or Extent object }
  \item{...}{Additional arguments, see under Details}  
}


\details{
Additional arguments that can be suplied to this function if \code{y} represents \bold{points}:

1) \code{method}. If \code{method='simple'} (the default), values for the cell a point falls in are returned. The 
alternative is \code{method='bilinear'}, in which case the returned values are interpolated from the values of the four nearest raster cells.

2) \code{buffer}. The radius of a buffer around each point for which to extract cell values. 
It can be a single value, or a vector of the length of the number of points. 
If the data are not projected (latitude/longitude), the unit should be meters. Otherwise it should be in map-units (typically also meters).

3) \code{fun}. Only useful when non-zero buffers are used. The values of all the cells are summarized with the function. The function should 
take a single numeric vector as argument and return a single values (e.g. mean, min or max). 

4) \code{na.rm}. Only useful when \code{fun} is supplied. If \code{na.rm=TRUE} (the default value), NA values are removed before fun is applied. 

Multi-layer Raster* objects accept these two additional arguments:

5) \code{layer}. Integer. First layer for which you want values 

6) \code{nl}. Integer. Number of layers for which you want values


Additional arguments that can be suplied to this function if \code{y} represents \bold{polygons}:
1) \code{fun}. Function to summarize the values (e.g. \code{mean})

If no \code{fun} argument is supplied:

2) \code{weights}. If \code{TRUE}, the function returns, for each polygon, a matrix with the cell values and the approximate fraction of each cell that is covered by the polygon(rounded to 1/100). The weights can be used for averaging; see examples. This option can be useful if the polygons are small relative to the cells size of the RasterLayer

3) \code{cellnumbers}. If \code{cellnumbers=TRUE} and \code{weights=TRUE}, cellnumbers will also be returned. 


Additional arguments that can be suplied to this function if \code{y} represents \bold{lines}:
\code{fun}. Function to summarize the values (e.g. \code{mean})
}


\value{
Vector, matrix, or list
}

\author{Robert J. Hijmans}

\examples{

r <- raster(ncol=36, nrow=18)
r[] <- 1:ncell(r)

###############################
# extract values by cell number
###############################
extract(r, 2:5)

###############################
# extract values with points
###############################
xy <- cbind(-50, seq(-80, 80, by=20))
xyValues(r, xy)
xyValues(r, xy, method='bilinear')

# examples with a buffer
xyValues(r, xy[1:3,], buffer=1000000)
xyValues(r, xy[1:3,], buffer=1000000, fun=mean)

## illustrating the varying size of a buffer (expressed in meters) on a longitude/latitude raster
 z <- xyValues(r, xy, buffer=1000000)
 s <- raster(r)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
## compare with raster that is not longitude/latitude
 projection(r) <- "+proj=UTM +zone=17" 
 xy[,1] <- 50
 z <- xyValues(r, xy, buffer=8)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
 plot(s)
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, add=TRUE)

###############################
# extract values with lines
###############################

cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2") ))

extract(r, lines)

###############################
# extract values with polygons
###############################
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))

#plot(r)
#plot(polys, add=TRUE)
v <- extract(r, polys)
v
# mean for each polygon
unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

# v <- polygonValues(r, polys, weights=TRUE)
# v
## weighted mean for each polygon
# unlist(lapply(v, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA  ))


###############################
# extract values with an extent
###############################
e <- extent(150,170,-60,-40)
extract(r, e)
#plot(r)
#plot(e, add=T)


}

\keyword{methods}
\keyword{spatial}

