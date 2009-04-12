# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
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


setMethod("plot", signature(x='BoundingBox', y='ANY'), 
	function(x, y, add=TRUE, ...)  {
		xy <- .bboxmatrix(x)
		xy[5,] <- xy[1,]
		if (add) {
			lines(xy, ...) 
		} else {
			plot(xy, type='l', ...)
		}
		if (!missing(y)) {
			if (class(y) == 'BoundingBox') {
				plot(x=y, add=TRUE, ...)
			}
		}
	}
)	

