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
		value <- xyValues(object, xyCoords)
	} else {
		cell <- cellFromXY(object, xyCoords)
		if (class(object) == 'RasterStack') {
			value <- values(object)[cell,]
		} else {
			value <- values(object)[cell]
		}
	}	
	value <- t(matrix(value))
	if (class(object) == 'RasterStack') {
		colnames(value) <- layerNames(object)
	} else {
		if (n==1) {
			colnames(value) <- 'value'
		} else {
			colnames(value) <- paste('value', 1:n, sep="")
		}
	}
	
	if (xy) { 
		value <- cbind(xyCoords, value)
		colnames(value)[1] <- 'x'
		colnames(value)[2] <- 'y'
	} 
	return(t(value))
	
}
