# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



if (!isGeneric("extent")) {
	setGeneric("extent", function(x, ...)
		standardGeneric("extent"))
}	

setMethod('extent', signature(x='BoundingBox'), 
	function(x){ return(x) }
)

setMethod('extent', signature(x='BasicRaster'), 
	function(x){ return(x@bbox) }
)

setMethod('extent', signature(x='Spatial'), 
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

setMethod('extent', signature(x='matrix'), 
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
	
setMethod('extent', signature(x='numeric'), 
	function(x, ...){ 
		dots <- unlist(list(...))
		x <- c(x, dots)
		if (length(x) < 4) {
			stop('insufficient number of elements (should be 4)')
		}
		if (length(x) > 4) {
			warning('more elements than expected (should be 4)')
		}
		bb <- new('BoundingBox')
		bb@xmin <- x[1]
		bb@xmax <- x[2]
		bb@ymin <- x[3]
		bb@ymax <- x[4]
		return(bb)
	}	
)

