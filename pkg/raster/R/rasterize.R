# Author: Robert J. Hijmans
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("rasterize")) {
	setGeneric("rasterize", function(x, y, ...)
		standardGeneric("rasterize"))
}	


setMethod('rasterize', signature(x='matrix', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...){ 
	return( .pointsToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, na.rm=na.rm,	...))
})


setMethod('rasterize', signature(x='data.frame', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...){ 
	x <- as.matrix(x)
	return( .pointsToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, na.rm=na.rmTRUE, ...))
})


setMethod('rasterize', signature(x='SpatialPoints', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...){ 
	return( .pointsToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, na.rm=na.rm, ...))
})


setMethod('rasterize', signature(x='SpatialLines', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", ...){ 
	.linesToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, ...)
})


setMethod('rasterize', signature(x='SpatialPolygons', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", getCover=FALSE, silent=FALSE, ...){ 
	.polygonsToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, getCover=getCover, silent=silent, ...)
})

setMethod('rasterize', signature(x='Extent', y='Raster'), 
function(x, y, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", getCover=FALSE, silent=TRUE, ...){ 
		y <- as(y, 'SpatialPolygons')
		.polygonsToRaster(x, y, field, fun=fun, background=background, mask=mask, update=update, updateValue=updateValue, filename=filename, getCover=getCover, silent=silent,...)
	}
)
