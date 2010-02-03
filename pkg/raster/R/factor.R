# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("is.factor")) {
	setGeneric("is.factor", function(x)
		standardGeneric("is.factor"))
}	

setMethod('is.factor', signature(x='RasterLayer'), 
	function(x) {
		return(x@data@isfactor)
	}
)


if (!isGeneric("make.factor")) {
	setGeneric("make.factor", function(x, ...)
		standardGeneric("make.factor"))
}

setMethod('make.factor', signature(x='ANY'), 
	function(x, ...) {
		return(factor(x, ...))
	}
)

setMethod('make.factor', signature(x='RasterLayer'), 
	function(x, levels=NULL, ...) {
		x@data@isfactor = TRUE
		if (! is.null(levels)) {
			x@data@levels = levels
		}
		return(x)
	}
)

setMethod('make.factor', signature(x='RasterStack'), 
	function(x, v=1, levels=NULL, ...) {
		x@layers[[v]]@data@isfactor = TRUE
		if (!is.null(levels)) {
			x@layers[[v]]@data@levels = levels
		}
		return(x)
	}
)

