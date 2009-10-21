# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

 

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, ...)
		standardGeneric('writeRaster')) 
	}
    
	

setMethod('writeRaster', signature(x='RasterBrick'), 
function(x, filename='', bandorder='BIL', ...) {

	if (substr(dataContent(x), 1, 3) == 'row' ) {
		return( .writeBrickRow(x, filename=filename, bandorder=bandorder, ...) )
	} else if (substr(dataContent(x), 1, 3) == 'all' ) {
		return( .writeBrick(x, filename=filename, bandorder=bandorder, ...) )
	} else {
		stop('cannot write data')
	}
}
)


setMethod('writeRaster', signature(x='RasterStack'), 
function(x, filename, bandorder='BIL', ...) {
    filename <- trim(filename)
	if (filename == '') {
		stop('you must supply a filename')
	}
	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	progress <- .progress(...)

	if (filetype != 'raster') {
		stop('Only "raster" format is currently supported for writing multiband files')
	}
	
	return( .writeStack(x, filename, bandorder=bandorder, datatype=datatype, filetype=filetype, overwrite=overwrite, progress=progress) )
}
)


	
setMethod('writeRaster', signature(x='RasterLayer'), 
function(x, filename='', ...) {

	filetype <- .filetype(...)

	if (! dataContent(x) %in% c('row', 'rows', 'all', 'sparse') ) {
		stop('No usable data available for writing.')
	}
	
	if (.isNativeDriver(filetype)) {
		if (substr(dataContent(x), 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, filetype, ...)
		} else {
			overwrite <- .overwrite(...)
			x <- .writeRasterAll(x, filetype, overwrite=overwrite)
		}  
	} else if (filetype=='ascii') {
		overwrite <- .overwrite(...)
		x <- .writeAscii(x, overwrite=overwrite)
	} else if (filetype=='CDF') {
		overwrite <- .overwrite(...)
		x <- .writeRasterCDF(x, overwrite=overwrite)
		
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

