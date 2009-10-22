# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

 

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, ...)
		standardGeneric('writeRaster')) 
	}
    
	

setMethod('writeRaster', signature(x='RasterLayer'), 
function(x, ...) {

	filetype <- .filetype(...)
	
	if (! dataContent(x) %in% c('row', 'rows', 'all', 'sparse') ) {
		stop('No usable data available for writing.')
	}
	
	if (.isNativeDriver(filetype)) {
		if (substr(dataContent(x), 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, ...)
		} else {
			x <- .writeRasterAll(x, filetype=filetype, ...)
		}  
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, ...)
	} else if (filetype=='CDF') {
		x <- .writeRasterCDF(x, ...)
		
	} else { # try rgdal
		overwrite <- .overwrite(...)
		if (dataContent(x) == 'row' ) {
			x <- .writeGDALrow(x, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		} else {
			x <- .writeGDALall(x, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		}  
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterBrick'), 
function(x, ...) {
	if (substr(dataContent(x), 1, 3) == 'row' ) {
		return( .writeBrickRow(object=x, ...) )
	} else if (substr(dataContent(x), 1, 3) == 'all' ) {
		return( .writeBrick(object=x, ...) )
	} else {
		stop('cannot write data')
	}
}
)


setMethod('writeRaster', signature(x='RasterStack'), 
function(x, ...) {
	return( .writeStack(x, ...) )
}
)

