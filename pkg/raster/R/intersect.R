# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

	
if (!isGeneric("intersect")) {
	setGeneric("intersect", function(x, y)
		standardGeneric("intersect"))
}	


setMethod('intersect', signature(x='Extent', y='Extent'), 
function(x, y) {
	
	intersectExtent(x, y, validate=TRUE)
	x@xmin <- max(x@xmin, y@xmin)
	x@xmax <- min(x@xmax, y@xmax)
	x@ymin <- max(x@ymin, y@ymin)
	x@ymax <- min(x@ymax, y@ymax)

	if ((x@xmax <= x@xmin) | (x@ymax <= x@ymin) ) {
		warning('Objects do not intersect')
		return(NULL)
	}
	return(x)
	
} )


