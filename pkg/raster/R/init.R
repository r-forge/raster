# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



init <- function(raster, fun=runif, filename="", ...) {

	outraster <- raster(raster, filename)
	dataType(outraster) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	if (!canProcessInMemory(outraster, 2) && filename == '') {
		filename <- rasterTmpFile()
		filename(outraster) <- filename
		if (getOption('verbose')) { cat('writing raster to:', filename(raster))	}						
	}
	if ( filename == '') {
		n <- ncell(raster)
		outraster <- setValues(outraster, fun(n)) 
	} else  {
		starttime <- proc.time()
		pb <- .setProgressBar(nrow(raster), type=.progress(...))
		
		n <- ncol(raster)
		for (r in 1:nrow(raster)) {
			outraster <- setValues(outraster, fun(n), r) 
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			.doProgressBar(pb, r)
		}	
		.closeProgressBar(pb, starttime)
	}
	return(outraster)
}

