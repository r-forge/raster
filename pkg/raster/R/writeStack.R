# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

writeStack <- function(rstack, filename, bandorder='BIL', ...) {

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) {
		stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'")
	}

	nl <- nlayers(rstack)
	rout <- raster(rstack)
	filename(rout) <- filename
	rout@file@nbands <- nl
	rout@file@bandorder <- bandorder
	dataType(rout) <- datatype

	if (bandorder=='BIL') {
		ncol(rout) <- ncol(rout) * nl
		if (dataContent(rstack) == 'all') {
			for (r in 1:nrow(rstack)) {
				rv <- valuesRow(rstack, r)
				rout <- setValues(rout, as.vector(rv), r)
				rout <- writeRaster(rout,  overwrite=overwrite)			
			}
		} else {
			for (r in 1:nrow(rstack)) {
				rstack <- readRow(rstack, r)
				rout <- setValues(rout, as.vector(values(rstack)), r)
				rout <- writeRaster(rout,  overwrite=overwrite)
			}
		}
	} else 	if (bandorder=='BIP') {
		ncol(rout) <- ncol(rout) * nl
		if (dataContent(rstack) == 'all') {
			sv <- as.vector(t(values(rstack)))
			rout <- setValues(rout, sv)
			rout <- writeRaster(rout, overwrite=overwrite)			
		} else {
			for (r in 1:nrow(rstack)) {
				rstack <- readRow(rstack, r)
				rout <- setValues(rout, as.vector(t(values(rstack))), r)
				rout <- writeRaster(rout, overwrite=overwrite)
			}
		}
	} else 	if (bandorder=='BSQ') {
		nrow(rout) <- nrow(rout) * nl
		if (dataContent(rstack) == 'all') {
			rout <- setValues(rout, as.vector(values(rstack)))
			rout <- writeRaster(rout, overwrite=overwrite)			
		} else {
			fakerow <- 0
			for (i in 1:nl) {
				sr <- raster(rstack, i)
				for (r in 1:nrow(sr)) {
					fakerow <- fakerow + 1
					sr <- readRow(sr, r)
					rout <- setValues(rout, values(sr, r), fakerow)
					rout <- writeRaster(rout, overwrite=overwrite)
				}
			}
		}		
	}
	nrow(rout) <- nrow(rstack)
	ncol(rout) <- ncol(rstack)
	rout@data@min <- minValue(rstack, -1)
	rout@data@max <- maxValue(rstack, -1)
	writeHeader(rout, type='raster')
	return(invisible(rstack))
}


