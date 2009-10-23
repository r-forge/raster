# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

 

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
	}
    
	

setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, ...) {

	filetype <- .filetype(...)
	
	if (! dataContent(x) %in% c('row', 'rows', 'all', 'sparse') ) {
		stop('No usable data available for writing.')
	}
	
	if (.isNativeDriver(filetype)) {
		if (substr(dataContent(x), 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, filename=filename, ...)
		} else {
			x <- .writeRasterAll(x, filename=filename, ...)
		}  
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename, ...)
	} else if (filetype=='CDF') {
		x <- .writeRasterCDF(x, filename=filename, ...)
		
	} else { 
		if (dataContent(x) == 'row' ) {
			x <- .writeGDALrow(x, filename=filename, ...)
		} else {
			x <- .writeGDALall(x, filename=filename, ...)
		}  
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterBrick', filename='character'), 
function(x, filename, bandorder='BIL', ...) {
	if (substr(dataContent(x), 1, 3) == 'row' ) {
		return( .writeBrickRow(object=x, filename=filename, bandorder=bandorder, ...) )
	} else if (substr(dataContent(x), 1, 3) == 'all' ) {
		return( .writeBrick(object=x, filename=filename, bandorder=bandorder, ...) )
	} else {
		stop('cannot write data')
	}
}
)


setMethod('writeRaster', signature(x='RasterStack', filename='character'), 
function(x, filename, bandorder='BIL', ...) {
	return( .writeStack(x, filename=filename, bandorder=bandorder, ...) )
}
)
