# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



'res<-' <- function(object, value) {
	if (length(value) == 1) {
		return( .setRes(object, xres=value, yres=value) )
	} else {
		return( .setRes(object, xres=value[1], yres=value[2]) )
	}
}


.setRes <- function(object, xres, yres=xres) {
	bb <- extent(object)
	nc <- round( (bb@xmax - bb@xmin) / xres )
	nr <- round( (bb@ymax - bb@ymin) / yres )
	if (nr != object@nrows | nc != object@ncols) {
		if (extends(class(object), "Raster")) {
			object <- clearValues(object)
		}
	}
	bb@xmax <- bb@xmin + nc * xres
	bb@ymin <- bb@ymax - nr * yres
	object	<- setExtent(object, bb)
	rowcol(object) <- c(nr, nc)
	return(object)
}

