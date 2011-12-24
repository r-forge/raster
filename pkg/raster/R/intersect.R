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
	e@xmin <- max(x@xmin, y@xmin)
	e@xmax <- min(x@xmax, y@xmax)
	e@ymin <- max(x@ymin, y@ymin)
	e@ymax <- min(x@ymax, y@ymax)

	if ((e@xmax <= e@xmin) | (e@ymax <= e@ymin) ) {
		warning('Objects do not intersect')
		return(NULL)
	}
	return(e)
	
} )


