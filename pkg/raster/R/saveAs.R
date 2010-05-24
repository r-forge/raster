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
	if (dataContent(x) == 'all') {
		return(  writeRaster(x, filename=filename, ...) )
	} 

	
	filename <- trim(filename)
	filetype <- .filetype(format=.filetype(...), filename=filename)
	filename <- .getExtension(filename, filetype)

	if ( trim(filename(x)) == filename ) {
		stop('filenames of source and destination should be different')
	}

	r <- raster(x)
	tr <- blockSize(r)
	pb <- pbCreate(tr$n, type=.progress(...))			
	r <- writeStart(r, filename=filename, ...)
	for (i in 1:tr$n) {
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		r <- writeValues(r, v, tr$row[i])
		pbStep(pb, i) 			
	}
	r <- writeStop(r)
	pbClose(pb)
	return(r)
}
)



setMethod('saveAs', signature(x='RasterStackBrick', filename='character'), 
	function(x, filename, bandorder='BIL', ...) {

		filename <- trim(filename)
		if ( toupper(x@file@name == toupper(filename) )) {
			stop('filenames of source and destination should be different')
		}
		filetype <- .filetype(...)
		
		if (filetype=='raster') {
			return( .writeBrick(object=x, filename=filename, bandorder=bandorder, ...) )
		} else {
			b <- brick(x)
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, type=.progress())
			b <- writeStart(b, filename=filename, bandorder=bandorder, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$size)
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
		}
	}
)


