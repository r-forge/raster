# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.8
# Licence GPL v3


saveAs <- function(raster, filename, filetype='raster', datatype='FLT4S', overwrite=FALSE, result=TRUE) {
	
	if (dataContent(raster) == 'all') {
		dataType(raster) <- datatype
		filename(raster) <- filename
		raster <- writeRaster(raster, filetype=filetype, overwrite=overwrite)
		return(raster)
	} 

	if ( trim(filename(raster)) == trim(filename) ) {
		stop('filenames should be different')
	}

# if filetype and datatype are the same, then use copyRasterfile 
	newr <- raster(raster, filename)
	dataType(newr) <- datatype
	for (r in 1:nrow(newr)) {
		raster <- readRow(raster, r)
		newr <- setValues(newr, values(raster), r)
		newr <- writeRaster(newr, filetype=filetype, overwrite=overwrite)
	}
	if (result) {
		return(newr)
	} else {
		return(invisible())	
	}
}

