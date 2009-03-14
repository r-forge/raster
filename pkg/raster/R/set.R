# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

.addHistory <- function(raster, message) {
	if (is.character(message) & message != "") {
		raster@history <- c(message, raster@history)
	}	
}


setProjection <- function(x, value) {
	if (class(value)=="CRS") {
		x@crs <- value
	} else {	
		x@crs <- newCRS(value)
	}	
	return(x)
}



roundCoords <- function(object, digits=0) {
	digits <- max(0, digits)
	b <- getBbox(object)
	b@xmin <- round(b@xmin, digits)
	b@xmax <- round(b@xmax, digits)
	b@ymin <- round(b@ymin, digits)
	b@ymax <- round(b@ymax, digits)
	if (class(object) == 'BoundingBox') {
		return(b)
	}
	object <- setExtent(object, b)
	return(object)
}

.nudgeCoords <- function(bb){
	bb <- getBbox(bb)
	bb@xmin <- floor(bb@xmin)
	bb@ymin <- floor(bb@ymin)
	bb@xmax <- ceiling(bb@xmax)
	bb@ymax <- ceiling(bb@ymax)
	return(bb)
}


setNAvalue <- function(raster, value) {
	raster@file@nodatavalue <- value
	return(raster)
}
