# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("extract")) {
	setGeneric("extract", function(x, y, ...)
		standardGeneric("extract"))
}	


setMethod('extract', signature(x='Raster', y='vector'), 
function(x, y, ...){ 
	return(cellValues(x, y, ...))
})


setMethod('extract', signature(x='Raster', y='matrix'), 
function(x, y, ...){ 
	return(xyValues(x, y, ...))
})


setMethod('extract', signature(x='Raster', y='data.frame'), 
function(x, y, ...){ 
	return(xyValues(x, y, ...))
})


setMethod('extract', signature(x='Raster', y='SpatialPoints'), 
function(x, y, ...){ 
	return(xyValues(x, y, ...))
})


setMethod('extract', signature(x='Raster', y='SpatialLines'), 
function(x, y, ...){ 
	.lineValues(x, y, ...)
})



setMethod('extract', signature(x='Raster', y='SpatialPolygons'), 
function(x, y, ...){ 
	.polygonValues(x, y, ...)
})



setMethod('extract', signature(x='Raster', y='Extent'), 
function(x, y, ...){ 
	return( getValues(crop(x, y, ...)) )
})

