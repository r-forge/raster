# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

init <- function(raster, fun=runif, filename="", ...) {

	outraster <- raster(raster)
	filename <- trim(filename)
	
	if (!canProcessInMemory(outraster, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}
	if ( filename == '') {
		n <- ncell(raster)
		outraster <- setValues(outraster, fun(n)) 
	} else  {
		n <- ncol(raster)
		for (r in 1:nrow(raster)) {
			outraster <- setValues(outraster, fun(n), r) 
			outraster <- writeRaster(outraster, filename=filename, doPB=TRUE, ...)
		}	
	}
	return(outraster)
}

