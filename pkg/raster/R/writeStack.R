# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

.writeStack <- function(object, filename, bandorder, filetype, datatype, overwrite, progress='') {

	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) {
		stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'")
	}

	nl <- nlayers(object)
	rout <- raster(raster(object)) 
	filename(rout) <- filename
	rout@file@nbands <- nl
	rout@file@bandorder <- bandorder
	dataType(rout) <- datatype

	starttime <- proc.time()
	pb <- pbSet(nrow(rout), type=progress)

	if (bandorder=='BIL') {
		ncol(rout) <- ncol(rout) * nl
		for (r in 1:nrow(object)) {
			vals <- getValues(object, r)
			rout <- setValues(rout, as.vector(vals), r)
			rout <- writeRaster(rout,  overwrite=overwrite)
			pbDo(pb, r) 				
		}
	} else 	if (bandorder=='BIP') {
		ncol(rout) <- ncol(rout) * nl
		for (r in 1:nrow(object)) {
			vals <- getValues(object, r)
			rout <- setValues(rout, as.vector(t(vals)), r)
			rout <- writeRaster(rout, overwrite=overwrite)
			pbDo(pb, r) 				
		}
	} else 	if (bandorder=='BSQ') {
		nrow(rout) <- nrow(rout) * nl
		fakerow <- 0
		for (i in 1:nl) {
			sr <- raster(object, i)
			for (r in 1:nrow(sr)) {
				fakerow <- fakerow + 1
				vals <- getValues(sr, r)
				rout <- setValues(rout, vals, fakerow)
				rout <- writeRaster(rout, overwrite=overwrite)
				pbDo(pb, r) 				
			}
		}		
	}
	pbClose(pb, starttime)
	nrow(rout) <- nrow(object)
	ncol(rout) <- ncol(object)
	rout@data@min <- minValue(object, -1)
	rout@data@max <- maxValue(object, -1)

	rout@layernames <- layerNames(object)

	writeRasterHdr(rout, filetype)

	return(invisible(brick(filename(rout))))
}


