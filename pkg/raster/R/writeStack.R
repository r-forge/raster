# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

.writeStackBrick <- function(object, filename, bandorder, filetype, datatype, overwrite) {

	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) {
		stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'")
	}

	nl <- nlayers(object)
	rout <- raster(object)
	filename(rout) <- filename
	rout@file@nbands <- nl
	rout@file@bandorder <- bandorder
	dataType(rout) <- datatype

	if (bandorder=='BIL') {
		ncol(rout) <- ncol(rout) * nl
		if (dataContent(object) == 'all') {
			for (r in 1:nrow(object)) {
				rv <- getValues(object, r)
				rout <- setValues(rout, as.vector(rv), r)
				rout <- writeRaster(rout,  overwrite=overwrite)			
			}
		} else {
			for (r in 1:nrow(object)) {
				object <- readRow(object, r)
				rout <- setValues(rout, as.vector(values(object)), r)
				rout <- writeRaster(rout,  overwrite=overwrite)
			}
		}
	} else 	if (bandorder=='BIP') {
		ncol(rout) <- ncol(rout) * nl
		if (dataContent(object) == 'all') {
			sv <- as.vector(t(values(object)))
			rout <- setValues(rout, sv)
			rout <- writeRaster(rout, overwrite=overwrite)			
		} else {
			for (r in 1:nrow(object)) {
				object <- readRow(object, r)
				rout <- setValues(rout, as.vector(t(values(object))), r)
				rout <- writeRaster(rout, overwrite=overwrite)
			}
		}
	} else 	if (bandorder=='BSQ') {
		nrow(rout) <- nrow(rout) * nl
		if (dataContent(object) == 'all') {
			rout <- setValues(rout, as.vector(values(object)))
			rout <- writeRaster(rout, overwrite=overwrite)			
		} else {
			fakerow <- 0
			for (i in 1:nl) {
				sr <- raster(object, i)
				for (r in 1:nrow(sr)) {
					fakerow <- fakerow + 1
					sr <- readRow(sr, r)
					rout <- setValues(rout, values(sr, r), fakerow)
					rout <- writeRaster(rout, overwrite=overwrite)
				}
			}
		}		
	}
	nrow(rout) <- nrow(object)
	ncol(rout) <- ncol(object)
	rout@data@min <- minValue(object, -1)
	rout@data@max <- maxValue(object, -1)
	writeHeader(rout, type='raster')
	return(invisible(object))
}


