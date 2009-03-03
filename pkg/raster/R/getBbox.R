# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



.bboxmatrix <- function(x) {
	xy <- matrix(NA, nrow=5, ncol=2)
	xy[1,1] <- x@xmin
	xy[1,2] <- x@ymax
	xy[2,1] <- x@xmax
	xy[2,2] <- x@ymax
	xy[3,1] <- x@xmax
	xy[3,2] <- x@ymin
	xy[4,1] <- x@xmin
	xy[4,2] <- x@ymin
	return(xy)
}



if (!isGeneric("getBbox")) {
	setGeneric("getBbox", function(x)
		standardGeneric("getBbox"))
}	

setMethod('getBbox', signature(x='BoundingBox'), 
	function(x){ return(x) }
)

setMethod('getBbox', signature(x='BasicRaster'), 
	function(x){ return(x@bbox) }
)

setMethod('getBbox', signature(x='Spatial'), 
	function(x){ 
		bndbox <- bbox(x)
		bb <- new('BoundingBox')
		bb@xmin <- bndbox[1,1]
		bb@xmax <- bndbox[1,2]
		bb@ymin <- bndbox[2,1]
		bb@ymax <- bndbox[2,2]
		return(bb) 
	}
)

setMethod('getBbox', signature(x='matrix'), 
	function(x){ 
		if (min(dim(x)) < 2) {
			stop('matrix should have dimensions of at least 2 by 2') }		
		bb <- new('BoundingBox')
		bb@xmin <- x[1,1]
		bb@xmax <- x[1,2]
		bb@ymin <- x[2,1]
		bb@ymax <- x[2,2]
		return(bb)
	}
)
	
setMethod('getBbox', signature(x='vector'), 
	function(x){ 
		if (length(x) < 4) {
			stop('vector supplied is too short')
		}
		if (length(x) > 4) {
			warning('vector supplied is longer then expected (should be 4)')
		}
		bb <- new('BoundingBox')
		bb@xmin <- x[1]
		bb@xmax <- x[2]
		bb@ymin <- x[3]
		bb@ymax <- x[4]
		return(bb)
	}	
)

