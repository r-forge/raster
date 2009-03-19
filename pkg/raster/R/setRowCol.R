# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setRowCol <- function(object, nrows=nrow(object), ncols=ncol(object)) {
	if (extends(class(object), "Raster")) {
		object <- clearValues(object)
		#object@data@source <- 'ram'
	}
	object@ncols <- as.integer(ncols)
	object@nrows <- as.integer(nrows)
	return(object)
}

setRes <- function(object, xres, yres=xres) {
	if (extends(class(object), "Raster")) {
		object <- clearValues(object)
		#object@data@source <- 'ram'
	}
	bb <- extent(object)
	nc <- round( (bb@xmax - bb@xmin) / xres )
	nr <- round( (bb@ymax - bb@ymin) / yres )
	bb@xmax <- bb@xmin + nc * xres
	bb@ymin <- bb@ymax - nr * yres
	object	<- setExtent(object, bb)
	object <- setRowCol(object, nr, nc)
	return(object)
}


