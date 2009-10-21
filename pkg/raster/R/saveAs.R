# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


saveAs <- function(raster, filename, filetype, datatype, overwrite, progress, result=TRUE) {
	
	if (missing(filetype)) { filetype <- .filetype()	} 
	if (missing(datatype)) { datatype <- .datatype() }
	if (missing(overwrite)) { overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }
	
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
	
	starttime <- proc.time()
	pb <- pbSet(nrow(newr), type=progress)
	
	for (r in 1:nrow(newr)) {
		raster <- readRow(raster, r)
		newr <- setValues(newr, values(raster), r)
		newr <- writeRaster(newr, filetype=filetype, overwrite=overwrite)
		pbDo(pb, r) 
	}
	pbClose(pb, starttime)
		
	if (result) {
		return(newr)
	} else {
		return(invisible())	
	}
}

