# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3


.writeBrick <- function(object, filename, bandorder='BIL', ...) {
#.writeBrick <- function(object, filename='', bandorder, ...) {
	
	filetype <- .filetype(...)
	if (filetype != 'raster') {
		stop('Only "raster" format is currently supported for writing multiband files')
	}

	filename <- trim(filename)
	
#	filetype <- .filetype(...)  # not used
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	progress <- .progress(...)

	if (filetype != 'raster') {
		stop('Only "raster" format is currently supported for writing multiband files')
	}

	
	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) {
		stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'")
	}

	nl <- nlayers(object)
	rout <- raster(object)
	rout@file@nbands <- nl
	rout@file@bandorder <- bandorder
	.setDataType(rout) <- datatype

	
	pb <- pbCreate(nrow(rout), type=progress)

	if (bandorder=='BIL') {
		ncol(rout) <- ncol(rout) * nl
		for (r in 1:nrow(object)) {
			rv <- getValues(object, r)
			rout <- setValues(rout, as.vector(rv), r)
			rout <- writeRaster(rout, filename=filename, datatype=datatype, overwrite=overwrite)
			pbStep(pb, r) 				
		}
	} else 	if (bandorder=='BIP') {
		ncol(rout) <- ncol(rout) * nl
		if (dataContent(object) == 'all') {
			sv <- as.vector(t(getValues(object)))
			object <- clearValues(object)
			rout <- setValues(rout, sv)
			rm(sv)
			rout <- writeRaster(rout, filename=filename, datatype=datatype, overwrite=overwrite)			
		} else {
			for (r in 1:nrow(object)) {
				rv <- getValues(object, r)
				rout <- setValues(rout, as.vector(t(rv)), r)
				rout <- writeRaster(rout, filename=filename, datatype=datatype, overwrite=overwrite)
				pbStep(pb, r) 				
			}
		}
	} else 	if (bandorder=='BSQ') {
		nrow(rout) <- nrow(rout) * nl
		if (dataContent(object) == 'all') {
			rout <- setValues(rout, as.vector(values(object)))
			object <- clearValues(object)
			rout <- writeRaster(rout, filename=filename, overwrite=overwrite)			
		} else {
			fakerow <- 0
			pb <- pbCreate(nrow(rout), type=progress)
			rr <- 0
			for (i in 1:nl) {
				sr <- raster(object, i)
				for (r in 1:nrow(sr)) {
					fakerow <- fakerow + 1
					rv <- getValues(sr, r)
					rout <- setValues(rout, rv, fakerow)
					rout <- writeRaster(rout, filename=filename, datatype=datatype, overwrite=overwrite)
					rr <- rr + 1
					pbStep(pb, rr) 				
				}				
			}
		}		
	}
	
	
	pbClose(pb)
	nrow(rout) <- nrow(object)
	ncol(rout) <- ncol(object)
	rout@data@min <- minValue(object, -1)
	rout@data@max <- maxValue(object, -1)
	rout@layernames <- layerNames(object)
	writeRasterHdr(rout, filetype=filetype)
	return(invisible(brick(filename(rout))))
}

