# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3


drawPoly <- function(col='red') {
	xy <- locator(n=10000, type="l", col=col)
	xy <- cbind(xy$x, xy$y)
	xy <- rbind(xy, xy[1,])
	lines(xy[(length(xy[,1])-1):length(xy[,1]),], col=col)
	return( SpatialPolygons(list(Polygons(list(Polygon(xy)), 1))) )
}


drawLine <- function(col='red') {
	xy <- locator(n=10000, type="l", col=col)
	xy <- cbind(xy$x, xy$y)
	return( SpatialLines(list(Lines(list(Line(xy)), "1"))) )
}


polygonFromBbox <- function(bndbox) {
	bb <- getBbox(bndbox)
	p <- rbind(c(bb@xmin, bb@ymin), c(bb@xmin, bb@ymax), c(bb@xmax, bb@ymax), c(bb@xmax, bb@ymin), c(bb@xmin, bb@ymin) )
	return( SpatialPolygons(list(Polygons(list(Polygon(p)), 1))) )
}

