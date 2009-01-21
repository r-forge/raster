# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3




newBbox <- function(xmn, xmx, ymn, ymx) {
	bb <- new('BoundingBox')
	bb@xmin <- xmn
	bb@xmax <- xmx
	bb@ymin <- ymn
	bb@ymax <- ymx
	return(bb)
}


setMethod('bbox', signature(obj='Raster'), 
	function(obj) {
		bb <- matrix(ncol=2, nrow=2)
		colnames(bb) <- c("min","max")
		rownames(bb) <- c("s1","s2")
		bb[1,1] <- xmin(obj)
		bb[1,2] <- xmax(obj)
		bb[2,1] <- ymin(obj)
		bb[2,2] <- ymax(obj)
		return(bb)
	}	
)


if (!isGeneric("getBbox")) {
	setGeneric("getBbox", function(object)
		standardGeneric("getBbox"))
}	

setMethod('getBbox', signature(object='BoundingBox'), 
	function(object){ return(object) }
)

setMethod('getBbox', signature(object='BasicRaster'), 
	function(object){ return(object@bbox) }
)

setMethod('getBbox', signature(object='Spatial'), 
	function(object){ 
		bndbox <- bbox(object)
		bb <- new('BoundingBox')
		bb@xmin <- bndbox[1,1]
		bb@xmax <- bndbox[1,2]
		bb@ymin <- bndbox[2,1]
		bb@ymax <- bndbox[2,2]
		return(bb) 
	}
)

setMethod('getBbox', signature(object='matrix'), 
	function(object){ 
		bb <- new('BoundingBox')
		bb@xmin <- object[1,1]
		bb@xmax <- object[1,2]
		bb@ymin <- object[2,1]
		bb@ymax <- object[2,2]
	}
)
	
setMethod('getBbox', signature(object='vector'), 
	function(object){ 
		if (length(object) < 4) {
			stop('vector supplied is too short')
		}
		if (length(object) > 4) {
			warning('vector supplied is longer then expected (should be 4)')
		}
		bb <- new('BoundingBox')
		bb@xmin <- object[1]
		bb@xmax <- object[2]
		bb@ymin <- object[3]
		bb@ymax <- object[4]
		return(bb)
	}	
)


setBbox <- function(object, bndbox, keepres=FALSE, snap=FALSE) {
	xrs <- xres(object)
	yrs <- yres(object)
	oldbb <- getBbox(object)
	bb <- getBbox(bndbox)
	
	if (snap) {
		bb@xmin <- max(bb@xmin, oldbb@xmin)
		bb@xmax <- min(bb@xmax, oldbb@xmax)
		bb@ymin <- max(bb@ymin, oldbb@ymin)
		bb@ymax <- min(bb@ymax, oldbb@ymax)
		col <- colFromX(object, bb@xmin)
		mn <- xFromCol(object, col) - 0.5 * xres(object)
		mx <- xFromCol(object, col) + 0.5 * xres(object)
		if (abs(bb@xmin - mn) > abs(bb@xmin - mx)) { bb@xmin <- mx } else { bb@xmin <- mn }
		col <- colFromX(object, bb@xmax)
		mn <- xFromCol(object, col) - 0.5 * xres(object)
		mx <- xFromCol(object, col) + 0.5 * xres(object)
		if (abs(bb@xmax - mn) > abs(bb@xmax - mx)) { bb@xmax <- mx } else { bb@xmax <- mn }
		row <- rowFromY(object, bb@ymin)
		mn <- yFromRow(object, row) - 0.5 * yres(object)
		mx <- yFromRow(object, row) + 0.5 * yres(object)
		if (abs(bb@ymin - mn) > abs(bb@ymin - mx)) { bb@ymin <- mx } else { bb@ymin <- mn }
		row <- rowFromY(object, bb@ymax)
		mn <- yFromRow(object, row) - 0.5 * yres(object)
		mx <- yFromRow(object, row) + 0.5 * yres(object)
		if (abs(bb@ymax - mn) > abs(bb@ymax - mx)) { bb@ymax <- mx } else { bb@ymax <- mn }
	}
	
	object@bbox <- bb
	if (keepres) {
		nc <- as.integer(round( (xmax(object) - xmin(object)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { object@ncols <- nc }
		nr <- as.integer(round( (ymax(object) - ymin(object)) / xrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { object@nrows <- nr }
		object@bbox@xmax <- object@bbox@xmin + ncol(object) * xrs
		object@bbox@ymax <- object@bbox@ymin + nrow(object) * yrs
	}
	return(object)
}



changeBbox <- function(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) {
	bb <- newBbox(xmn, xmx, ymn, ymx) 
	object <- setBbox(object, bb, keepres=keepres) 
	return(object)
}

