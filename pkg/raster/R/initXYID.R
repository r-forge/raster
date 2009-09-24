# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

initXYID <- function(raster, v='id', filename="", ...) {
	
	outraster <- raster(raster, filename)
	dataType(outraster) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	if (!( v %in% c()) ) {
		stop('v should be x, y, or id')
	}
	if (!canProcessInMemory(outraster, 2) && filename == '') {
		filename <- rasterTmpFile()
		filename(outraster) <- filename
		if (getOption('verbose')) { 
			cat('writing raster to:', filename(raster))	
		}
	}
	if ( filename == '') {
		n <- ncell(raster)
		if (v == 'id') {
			vals <- 1:ncell(outraster)
		} else if (v == 'x') {
			vals <- xFromCell(outraster, 1:ncell(outraster))
		} else if (v == 'y') {
			vals <- yFromCell(outraster, 1:ncell(outraster))
		} 		
		outraster <- setValues(outraster, vals) 
	} else  {
		starttime <- proc.time()
		pb <- .setProgressBar(nrow(raster), type=.progress(...))

		n <- ncol(raster)
		arow <- 1:n
		for (r in 1:nrow(raster)) {
			cells <- arow + (r-1) * n
			if (v == 'id') {
				vals <- cells
			} else if (v == 'x') {
				vals <- xFromCell(outraster, cells)
			} else if (v == 'y') {
				vals <- yFromCell(outraster, cells)
			} 		
			outraster <- setValues(outraster, vals, r) 
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			.doProgressBar(pb, r)
		}
		.closeProgressBar(pb, starttime)
	}	
	return(outraster)
}

