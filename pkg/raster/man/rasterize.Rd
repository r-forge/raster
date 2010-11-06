\name{rasterize}

\docType{methods}

\alias{rasterize}
\alias{rasterize,matrix,Raster-method}
\alias{rasterize,SpatialPoints,Raster-method}
\alias{rasterize,SpatialLines,Raster-method}
\alias{rasterize,SpatialPolygons,Raster-method}
\alias{rasterize,Extent,Raster-method}

\title{Rasterize points, lines, or polygons}

\description{
Rasterize d

This function has replaced four other functions in this package (pointsToRaster, linesToRaster, polygonsToRaster)
}

\usage{
rasterize(x, y, ...) 
}

\arguments{
  \item{x}{points (a SpatialPoints* object or a two-column matrix), SpatialLines*, or SpatialPolygons*, or an Extent object }
  \item{y}{Raster* object}
  \item{...}{Additional arguments, see under Details}  
}

\details{

Additional arguments

\bold{\code{field}}. The index (or column name) of the variable in the Spatial*DataFrame to be transfered. If \code{field < 0}, all features get the attribute value 1, and if \code{field == 0}, the attribute index is used (i.e. numbers from 1 to the number of features). In all cases you can also provide a vector with the same length as the number of spatial features, or a matrix where the number of rows matches the number of spatial features.
  
\bold{\code{fun}}. Determine what values to assign to cells overlapping with multiple spatial features. Either a functions such as mean, or one of the following character values: \code{'first', 'last', 'sum', 'min'}, or \code{'max'}

\code{background}. Value to put in the cells that do not cover any of the features of \code{x}. Default is \code{NA}

\code{mask}. Logical. If \code{TRUE} the values of the input Raster object are 'masked' by the spatial features of \code{x}. Cells that overlapwith the spatial features retain their values, the other cells become \code{NA}. Default is \code{FALSE}

\code{update}. Logical. If \code{TRUE}, the values of the Raster* object are updated for the cells that overlap the spatial features of \code{x}. Default is \code{FALSE}

\code{updateValue}. Character. Select areas to be updated with the polygon values (if \code{updateRaster == TRUE}) by the current cell values. Either \code{'all'}, \code{'NA'}, \code{'!NA'}, or \code{'zero'}. Default is \code{'NA'}

\code{filename}. Character. Output filename (optional)

\code{...} Additional arguments. See below.


\bold{= points =}

If \code{x} represents points, each point is assinged to a grid cell. The value of a grid cell is determined by the values associated with the points and function \code{fun}.
If you want to know the number of points in each grid cell, use the (default) \code{length} function. I.e., for each cell it computes the length
of the vector of points. For the sum of the values, use \code{sum}, for a yes/no result, you can use \code{fun=function(x){length(x)>0}}. For the number of unique values use \code{fun=function(x){length(unique(x))}}

Note that the function must take an \code{na.rm} argument, either explicitly or through 'dots'. This means that \code{fun=lenght} fails, but \code{fun=function(x,...)lenght(x)} works (but ignores the \code{na.rm} argument. To use the \code{na.rm} argument you can use a function like this \code{fun=function(x,na.rm){if (na.rm) length(na.omit(x)) else (length(x)}}, or use a function that removes NA values in all cases, like this function to compute the number of unique values "richness": \code{fun=function(x, ...){length(unique(na.omit(x)))}}. You can also pass multiple functions using a statement like \code{fun=function(x)c(lenght(x),mean(x))}, in which case the returned object is a RasterBrick (multiple layers).


There is one additional argument:

\code{na.rm}. Remove \code{NA} values? Default is \code{TRUE}


\bold{= lines =}

A polygon value is transferred to a raster-cell if it touches the cell. 

For SpatialLines objects the lines index is transferred (i.e. numbers from 1 to the number of polygons). This is also 
done for SpatialPolygonDataFrame objects when \code{field}==0. If  \code{field} < 0, all polygons get the value 1.



\bold{= polygons =}

If \code{x} is a SpatialPolygons* or an Extent object the following additional arguments are available.

\code{getCover}{Logical. If \code{TRUE}, the fraction of each grid cell that is covered by the polygons is returned (and the values of \code{field, overlap, mask}, and \code{update} are ignored. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell}    

\code{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed}    

A polygon value is transferred to a raster-cell if it covers the center of the cell. Either values associated with each polygon, or a polygon ID is transferred. Holes in polygons are recognized if they are correctly specified.



\bold{= all cases =}

The following additional arguments can be passed in all cases when saving the output to a file
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}


\value{
RasterLayer
}


\seealso{ \code{\link{values}} }


\author{Robert J. Hijmans}

\examples{

###############################
# rasterize points
###############################
r <- raster(ncols=36, nrows=18)
n <- 1000
x <- runif(n)* 360 - 180
y <- runif(n)* 180 - 90
xy <- cbind(x, y)
vals <- rep(1, n)
r <- rasterize(xy, r, vals)

# now with an sp SpatialPointsDataFrame object
p <- as.data.frame(cbind(xy, name=vals))
coordinates(p) <- ~x+y
r <- rasterize(p, r, 'name')


###############################
# rasterize lines
###############################
cds1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
cds2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
cds3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))

lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2"), Lines(list(Line(cds3)), "3") ))

r <- raster(ncols=90, nrows=45)
r <- rasterize(lines, r)

\dontrun{
plot(r)
plot(lines, add=TRUE)

r <- rasterize(lines, r, overlap='count')
plot(r)

r[] <- 1:ncell(r)
r <- rasterize(lines, r, mask=TRUE)
plot(r)
}

###############################
# rasterize polygons
###############################

p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))
pols <- SpatialPolygons( list(  Polygons(list(Polygon(p1), Polygon(hole)), 1), Polygons(list(Polygon(p2)), 2), Polygons(list(Polygon(p3)), 3)))
pols@polygons[[1]]@Polygons[[2]]@hole <- TRUE

r <- raster(ncol=180, nrow=90)
r <- rasterize(pols, r, overlap='sum')
#plot(r)
#plot(pols, add=T)

\dontrun{
## add a polygon
p5 <- rbind(c(-180,10), c(0,90), c(40,90), c(145,-10),  c(-25, -15), c(-180,0), c(-180,10))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(p5)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- rasterize(addpoly, r, field=1, updateRaster=TRUE, updateValue="NA")
plot(r2)
plot(pols, border="blue", lwd=2, add=TRUE)
plot(addpoly, add=TRUE, border="red", lwd=2)

## get the percentage cover of polygons in a cell
r3 <- raster(ncol=36, nrow=18)
r3 <- rasterize(pols, r3, getCover=TRUE)
}
}

\keyword{methods}
\keyword{spatial}

