# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
}


setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, format, ...) {

	filename <- .fullFilename(filename)
	if (filename == '') {
		stop('provide a filename')
	}
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			return( .saveAsRaster(x, filename, format=filetype, ...) )
		} else {
			stop('No usable data available for writing')
		}
	}

	if (.isNativeDriver(filetype)) {
		x <- .writeRasterAll(x, filename=filename, format=filetype, ...)
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename,...)
	} else if (filetype=='CDF') {
		x <- .rasterSaveAsNetCDF(x, filename=filename, ...)
	} else { 
		x <- .writeGDALall(x, filename=filename, format=filetype, ...)
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterStackBrick', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- .fullFilename(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			return( .saveAsBrick(x, filename, bandorder=bandorder, format=filetype, ...) )
		} else {
			stop('No cell values available for writing.')
		}
	}

	if (.isNativeDriver(filetype)) {
		return( .writeBrick(object=x, filename=filename, format=filetype, bandorder=bandorder, ...) )
	} else if (filetype=='CDF') {
		return ( .rasterSaveAsNetCDF(x, filename=filename, ...) )
	} else {
		return ( .writeGDALall(x, filename=filename, format=filetype, ...) )
	}
}
)

