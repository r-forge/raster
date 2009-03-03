# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3

###   xyValues   ###

if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(x, xyCoords)
		standardGeneric("xyValues"))
}	
	
setMethod("xyValues", signature(x='RasterLayer', xyCoords='matrix'), 
	function(x, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(x, xyCoords)
		return(.rasterReadCells(x, cells))
	}	
)	


setMethod("xyValues", signature(x='RasterStack', xyCoords='matrix'), 
	function(x, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(x, xyCoords)
		return(.stackReadCells(x, cells))
	}	
)



setMethod("xyValues", signature(x='RasterLayer', xyCoords='SpatialPoints'), 
	function(x, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(x, xyCoords)
		return(.rasterReadCells(x, cells))
	}	
)


setMethod("xyValues", signature(x='RasterStack', xyCoords='SpatialPoints'), 
	function(x, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(x, xyCoords)
		return(.stackReadCells(x, cells))
	}	
)

