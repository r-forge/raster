# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3


if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xyCoords, ...)
		standardGeneric("xyValues"))
}	
	
setMethod("xyValues", signature(object='RasterLayer', xyCoords='matrix'), 
	function(object, xyCoords, method='simple') { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; it should have 2 columns' )
		}
		if (method=='bilinear') {
			return(.bilinearValue(object, xyCoords))
		} else {
			cells <- cellFromXY(object, xyCoords)
			return(.rasterReadCells(object, cells))
		}
	}	
)	


setMethod("xyValues", signature(object='RasterStack', xyCoords='matrix'), 
	function(object, xyCoords, method='simple') { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		

		if (method == 'bilinear') {
			for (i in seq(nlayers(object))) {
				v <- .bilinearValue(object, xyCoords)
				if (i == 1) {
					result <- v
				} else {
					result <- cbind(result, v)
				}
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- object@data@colnames
			}	
			return(result)		
	
		} else {
			cells <- cellFromXY(object, xyCoords)
			return(.stackReadCells(object, cells))
		}
	}	
)



setMethod("xyValues", signature(object='RasterLayer', xyCoords='SpatialPoints'), 
	function(object, xyCoords, method='simple') { 
		xyCoords <- coordinates(xyCoords)
		callNextMethod(object, xyCoords, method)
	}	
)


setMethod("xyValues", signature(object='RasterStack', xyCoords='SpatialPoints'), 
	function(object, xyCoords, method='simple') { 
		xyCoords <- coordinates(xyCoords)
		callNextMethod(object, xyCoords, method)
	}	
)

