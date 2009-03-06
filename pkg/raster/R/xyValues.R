# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3


if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(raster, xyCoords)
		standardGeneric("xyValues"))
}	
	
setMethod("xyValues", signature(raster='RasterLayer', xyCoords='matrix'), 
	function(raster, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(raster, xyCoords)
		return(.rasterReadCells(raster, cells))
	}	
)	


setMethod("xyValues", signature(raster='RasterStack', xyCoords='matrix'), 
	function(raster, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(raster, xyCoords)
		return(.stackReadCells(raster, cells))
	}	
)



setMethod("xyValues", signature(raster='RasterLayer', xyCoords='SpatialPoints'), 
	function(raster, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(raster, xyCoords)
		return(.rasterReadCells(raster, cells))
	}	
)


setMethod("xyValues", signature(raster='RasterStack', xyCoords='SpatialPoints'), 
	function(raster, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(raster, xyCoords)
		return(.stackReadCells(raster, cells))
	}	
)

