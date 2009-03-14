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

setRowCol <- function(raster, nrows=nrow(raster), ncols=ncol(raster)) {
	raster <- clearValues(raster)
	raster@ncols <- as.integer(ncols)
	raster@nrows <- as.integer(nrows)
	return(raster)
}

setRes <- function(object, xres, yres=xres) {
	if (extends(class(object), "Raster")) {
		object <- clearValues(object)
	}
	bb <- getBbox(object)
	nc <- round( (bb@xmax - bb@xmin) / xres )
	nr <- round( (bb@ymax - bb@ymin) / yres )
	bb@xmax <- bb@xmin + nc * xres
	bb@ymin <- bb@ymax - nr * yres
	object	<- setExtent(object, bb)
	object <- setRowCol(object, nr, nc)
	return(object)
}

setRaster <- function(object, filename="", values=NULL) {
	warning('depracated, use "raster()" instead')
	return(raster(object, filename, values))
}

setFilename <- function(object, filename) {
	if (is.na(filename)) {filename <- ""}
	filename <- trim(filename)
	if (class(object)=='RasterStack') {
		object@filename <- setFileExtension(filename, ".stk")
	} else {
		object@file@name <- filename
	}	
	if (class(object)=='RasterLayer') {
		shortname <- shortFileName(filename)
		shortname <- setFileExtension(shortname, "")
		shortname <- gsub(" ", "_", shortname)
		if (nbands(object) > 1) { shortname <- paste(shortname, "_", band(object)) } 
		object@file@shortname <- shortname
		object@file@gdalhandle <- list()
	}	
	return(object)	
}




setProjection <- function(object, projs) {
	if (class(projs)=="CRS") {
		object@crs <- projs
	} else {	
		object@crs <- newCRS(projs)
	}	
	return(object)
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
