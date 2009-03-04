# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date: March 2009
# Version 0.8
# Licence GPL v3


if (!isGeneric("lines")) {
	setGeneric("lines", function(x, ...)
		standardGeneric("lines"))
}

#setMethod("lines", signature(x='ANY'), 
#	function(x, ...) {
#		graphics::lines(x, ...)
#	}
#)


setMethod("lines", signature(x='BoundingBox'), 
	function(x, ...)  {
		xy <- .bboxmatrix(x)
		xy[5,] <- xy[1,]
		lines(xy, ...)
	}
)	

