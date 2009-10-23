# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


saveAs <- function(raster, filename, ...) {

	filename <- trim(filename)
	if ( trim(filename(raster)) == filename ) {
		stop('filenames of source and destination should be different')
	}
	
	if (dataContent(raster) == 'all') {
		raster <- writeRaster(raster, filename=filename, ...)
		return(raster)
	} 

# to do: if filetype and datatype are the same, then just copy the file .... 

	newr <- raster(raster)
	for (r in 1:nrow(newr)) {
		raster <- readRow(raster, r)
		newr <- setValues(newr, values(raster), r)
		newr <- writeRaster(newr, filename=filename, ..., doPB=TRUE)
	}
	return(newr)
}

