# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


saveAs <- function(raster, filename, filetype, datatype, overwrite, progress, result=TRUE) {

	filename <- trim(filename)
	if ( trim(filename(raster)) == filename ) {
		stop('filenames should be different')
	}

	if (missing(filetype)) { filetype <- .filetype()	} 
	if (missing(datatype)) { datatype <- .datatype() }
	if (missing(overwrite)) { overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }
	
	if (dataContent(raster) == 'all') {
		raster <- writeRaster(raster, filename=filename, datatype=datatype, filetype=filetype, overwrite=overwrite)
		return(raster)
	} 

# if filetype and datatype are the same, then use copyRasterfile 
	newr <- raster(raster, filename)
	
	for (r in 1:nrow(newr)) {
		raster <- readRow(raster, r)
		newr <- setValues(newr, values(raster), r)
		newr <- writeRaster(newr, filename=filename, datatype=datatype, filetype=filetype, overwrite=overwrite, doPB=TRUE)
	}
		
	if (result) {
		return(newr)
	} else {
		return(invisible())	
	}
}

