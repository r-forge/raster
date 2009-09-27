# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

 

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, ...)
		standardGeneric('writeRaster')) 
	}	


setMethod('writeRaster', signature(x='RasterBrick'), 
function(x, filename, bandorder='BIL', ...) {
    filename <- trim(filename)
	if (filename == '') {
		filename <- filename(x)
		if (filename == '') {
			stop('RasterBrick has no filename; and no filename specified as argument to writeRaster')
		}
	}
	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	progress <- .progress(...)

	if (filetype != 'raster') {
		stop('Only "raster" format is currently supported for writing multiband files')
	}
	if (substr(dataContent(x), 1, 3) == 'row' ) {
		return( .writeBrickRow(x, filename=filename, bandorder=bandorder, datatype=datatype, filetype=filetype, overwrite=overwrite, progress=progress) )
	} else {
		return( .writeBrick(x, filename=filename, bandorder=bandorder, datatype=datatype, filetype=filetype, overwrite=overwrite, progress=progress) )
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
function(x, filename='', assign=FALSE, ...) {

	dataType(x) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	
	if (assign){ 
		raster_name <- deparse(substitute(x))
	}

	if (filename != '') {
		filename(x) <- filename
	}
	if (filename(x) == '') {
		stop('RasterLayer has no filename; and no filename specified as argument to writeRaster')
	}
	
	if (! dataContent(x) %in% c('row', 'rows', 'all', 'sparse') ) {
		stop('No usable data available for writing.')
	}
	
	if (filetype=='raster') {
		if (substr(dataContent(x), 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, overwrite=overwrite)
		} else {
			x <- .writeRasterAll(x, overwrite=overwrite)
		}  
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, overwrite=overwrite)
		
	} else if (filetype=='CDF') {
		x <- .writeRasterCDF(x, overwrite=overwrite)
		
	} else { 
		.isSupportedGDALFormat(filetype)
		if (dataContent(x) == 'row' ) {
			x <- .writeGDALrow(x, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		} else {
			x <- .writeGDALall(x, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		}  
	}
	if (assign) {
		assign(raster_name, x, envir=parent.frame())
		return(invisible())	
	} else {
		return(x)
	}
}	
)

