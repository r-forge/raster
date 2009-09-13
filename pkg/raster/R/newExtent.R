# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



newExtent <- function(xmn, xmx, ymn, ymx) {
	bb <- new('Extent')
	bb@xmin <- xmn
	bb@xmax <- xmx
	bb@ymin <- ymn
	bb@ymax <- ymx
	validObject(bb)
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

