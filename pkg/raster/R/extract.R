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
	y <- round(y)
	if (length(y) == 2) {
		warning("returning values at _cell numbers_ : ", y[1], " and ", y[2])
	}
	return( .cellValues(x, y, ...) )
})


setMethod('extract', signature(x='Raster', y='matrix'), 
function(x, y, ...){ 
	return( .xyValues(x, y, ...) )
})



setMethod('extract', signature(x='Raster', y='data.frame'), 
function(x, y, ...){ 
	return( .xyValues(x, as.matrix(y), ...))
})


setMethod('extract', signature(x='Raster', y='SpatialPoints'), 
function(x, y, ...){ 
	return( .xyValues(x, coordinates(y), ...))
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
	nlyrs <- nlayers(x)

	if (nlyrs > 1) {
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min(max(1, round(nl)), nlyrs-layer+1)
		x <- getValues(crop(x, y, ...))
		return( x[ , layer:(layer+nl-1)] )
	} else {
		return( getValues(crop(x, y, ...)) )
	}
	
})

#setMethod('extract', signature(x='Spatial', y='Spatial'), 
#function(x, y, ...){ 
#	return( overlay(x, y, ...) )
#})
