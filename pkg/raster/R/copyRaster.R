# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.8
# Licence GPL v3


saveAs <- function(raster, filename, filetype='raster', datatype='FLT4S', overwrite=FALSE) {
	
	if (dataContent(raster) == 'all') {
		dataType(raster) <- datatype
		filename(raster) <- filename
		writeRaster(raster, filetype=filetype, overwrite=overwrite)
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
		writeRaster(newr, filetype=filetype, overwrite=overwrite)
	}
	return(newr)
}


moveRasterFile <- function(raster, filename, overwrite=FALSE) {
	r <- copyRasterFile(raster, filename, overwrite)
	f <- trim(filename(raster))
	fileext <- toupper(fileExtension(f)) 
	if (fileext == ".GRD") {
		file.remove(f)
		fileExtension(f) <- '.gri'
		file.remove(f)
	}
	return(r)
}


copyRasterFile <- function(raster, filename, overwrite=FALSE) {
	f <- trim(filename(raster))
	fileext <- toupper(fileExtension(f)) 
	if (fileext == ".GRD") {
		fgrd <- filename
		fileExtension(fgrd) <- '.grd'
		fgri <- filename
		fileExtension(fgri) <- '.gri'
		if ( file.exists(fgrd) & !overwrite ) {
			stop('file exists & overwrite = FALSE')
		}
		if ( file.exists(fgri) & !overwrite ) {
			stop('file exists & overwrite = FALSE')
		}
		res <- file.copy(f, fgrd, overwrite)
		if (!res) { stop('could not copy grd file') } 
		fileExtension(f) <- ".gri"
		res <- file.copy(f, fgri, overwrite)
		if (!res) { stop('could not copy gri file') } 
		filename(raster) <- fgrd
		return(raster)
	} else {
		stop('only implemented for raster format files')
	}
}



