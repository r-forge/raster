# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3

BboxToPolygon <- function(bbox) {
	bbox <- getBbox(bbox)
	p <- rbind(c(bbox@xmin, bbox@ymin), c(bbox@xmin, bbox@ymax), c(bbox@xmax, bbox@ymax), c(bbox@xmax, bbox@ymin), c(bbox@xmin, bbox@ymin) )
	pol <- SpatialPolygons(list(Polygons(list(Polygon(p)), 1)))
	return(pol)
}

clickBbox <- function(show=TRUE, col="red") {
	loc <- locator(n=2, type="p")
	bb <- newBbox(min(loc$x), max(loc$x), min(loc$y), max(loc$y))
	if (show) {
		p <- rbind(c(bb@xmin, bb@ymin), c(bb@xmin, bb@ymax), c(bb@xmax, bb@ymax), c(bb@xmax, bb@ymin), c(bb@xmin, bb@ymin) )
		lines(p, col=col)
	}
	return(bb)
}


click <- function(object, n=1, xy=FALSE, type="n", ...) {
	loc <- locator(n, type, ...)
	x <- loc$x
	y <- loc$y
	xyCoords <- cbind(x, y)
	if (missing(object)) {
		return(cbind(xyCoords))
	}
	if (dataContent(object) != 'all') {
		if (dataSource(object) != 'disk') {
			stop('no data associated with this RasterLayer object')
		} else {
			value <- xyValues(object, xyCoords)
		}	
	} else {
		cell <- cellFromXY(object, xyCoords)
		value <- values(object)[cell]
	}	
	if (xy) { 
		return(cbind(xyCoords, value)) 
	} else {
		return(value)
	}
}

