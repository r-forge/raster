# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod("Math2", signature(x='Extent'), 
	function (x, digits=0) {
		digits <- max(0, digits)
		x@xmin <- callGeneric( x@xmin, digits)
		x@xmax <- callGeneric( x@xmax, digits)
		x@ymin <- callGeneric( x@ymin, digits)
		x@ymax <- callGeneric( x@ymax, digits)
		return(x)
	}
)


nudgeExtent <- function(x){
	b <- extent(x)
	b@xmin <- floor(b@xmin)
	b@ymin <- floor(b@ymin)
	b@xmax <- ceiling(b@xmax)
	b@ymax <- ceiling(b@ymax)
	if (class(x) == 'Extent') {
		return(b)
	}
	extent(x) <- b
	return(x)
}
