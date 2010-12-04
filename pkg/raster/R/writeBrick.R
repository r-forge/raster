# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

.writeBrick <- function(object, filename, bandorder='BIL', format='raster', ...) {
	filename <- trim(filename)
	if (filename == '') { stop('you must supply a filename') }

	if (! format %in% c("raster", "BIL", "BSQ", "BIP") ) {
		stop('format should be one of "raster", "BIL", "BSQ", "BIP" (or a rgdal supported format)')
	}
	if ( format %in% c("BIL", "BSQ", "BIP") ) { bandorder <- format }
	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) { stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'") }
	
	out <- brick(object, values=FALSE)
	nl <- out@data@nlayers

	out <- writeStart(out, filename, bandorder=bandorder, ...)
	
	if (inMemory(object)) {
		out <- writeValues(out, getValues(object))
	} else {
		tr <- blockSize(object)
		pb <- pbCreate(tr$n, type=.progress(...))
		for (i in 1:tr$n) {
			out <- writeValues(out, getValues(object, tr$row[i], tr$nrows[i]), tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
	}
	
	out <- writeStop(out)
	return( out )
}

