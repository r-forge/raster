# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("saveAs")) {
	setGeneric("saveAs", function(x, filename, ...)
		standardGeneric("saveAs"))
}	


setMethod('saveAs', signature(x='RasterLayer', filename='character'), 
function(x, filename, ...) {

	filename <- trim(filename)
	if ( trim(filename(x)) == filename ) {
		stop('filenames of source and destination should be different')
	}
	
	if (dataContent(x) == 'all') {
		x <- writeRaster(x, filename=filename, ...)
		return(x)
	} 
# to do: if filetype and datatype are the same, then just copy the file .... 
	newr <- raster(x)
	for (r in 1:nrow(newr)) {
		x <- readRow(x, r)
		newr <- setValues(newr, values(x), r)
		newr <- writeRaster(newr, filename=filename, ..., doPB=TRUE)
	}
	return(newr)
}
)

setMethod('saveAs', signature(x='RasterBrick', filename='character'), 
function(x, filename, ...) {

	filename <- trim(filename)
	if ( trim(filename(x)) == filename ) {
		stop('filenames of source and destination should be different')
	}
	
	if (dataContent(x) == 'all') {
		x <- writeRaster(x, filename=filename, ...)
		return(x)
	} 
# to do: if filetype and datatype are the same, then just copy the file .... 
	newr <- brick(x)
	for (r in 1:nrow(newr)) {
		x <- readRow(x, r)
		newr <- setValues(newr, values(x), r)
		newr <- writeRaster(newr, filename=filename, ..., doPB=TRUE)
	}
	return(newr)
}
)



setMethod('saveAs', signature(x='RasterStack', filename='character'), 
function(x, filename, ...) {
	newr <- brick(x)
	for (r in 1:nrow(newr)) {
		newr <- setValues(newr, getValues(x, r), r)
		newr <- writeRaster(newr, filename=filename, ..., doPB=TRUE)
	}
	return(newr)
}
)
