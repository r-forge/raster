# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3




click <- function(object, n=1, xy=FALSE, type="n", ...) {
	loc <- locator(n, type, ...)
	xyCoords <- cbind(loc$x, loc$y)
	if (missing(object)) {
		return(cbind(xyCoords))
	}
	if (dataContent(object) != 'all') {
		#if (dataSource(object) != 'disk') {
		#	stop('no data associated with this RasterLayer object')
		#} else {
		value <- xyValues(object, xyCoords)
		#}	
	} else {
		cell <- cellFromXY(object, xyCoords)
		if (class(object) == 'RasterStack') {
			value <- values(object)[cell,]
		} else {
			value <- values(object)[cell]
		}
	}	
	if (xy) { 
		return(cbind(xyCoords, value)) 
	} else {
		return(value)
	}
}
