# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

initXYID <- function(raster, v='id', filename="", overwrite=FALSE, datatype = 'FLT4S', filetype='raster', track=-1) {
	
	outraster <- raster(raster, filename)
	dataType(outraster) <- datatype
	
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
		}	
		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
	} 
	return(outraster)
}



init <- function(raster, fun=runif, filename="", overwrite=FALSE, datatype = 'FLT4S', filetype='raster', track=-1) {

	outraster <- raster(raster, filename)
	dataType(outraster) <- datatype
	
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
		n <- ncol(raster)
		for (r in 1:nrow(raster)) {
			outraster <- setValues(outraster, fun(n), r) 
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}	
		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
	}
	return(outraster)
}

