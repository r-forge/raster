# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



init <- function(raster, fun=runif, filename="", ...) {

	outraster <- raster(raster, filename)
	.setDataType(outraster) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	if (!canProcessInMemory(outraster, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename(raster))	}						
	}
	if ( filename == '') {
		n <- ncell(raster)
		outraster <- setValues(outraster, fun(n)) 
	} else  {
		
		pb <- pbCreate(nrow(raster), type=.progress(...))
		
		n <- ncol(raster)
		for (r in 1:nrow(raster)) {
			outraster <- setValues(outraster, fun(n), r) 
			outraster <- writeRaster(outraster, filename=filename, filetype=filetype, overwrite=overwrite)
			pbStep(pb, r)
		}	
		pbClose(pb)
	}
	return(outraster)
}

