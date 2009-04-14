# Author: Robert J. Hijmans
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


disaggregate <- function(raster, fact=2, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {
	if (length(fact)==1) {
		fact <- round(fact)
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- round(fact[1])
		yfact <- round(fact[2])
		if (xfact < 2) { stop('fact[1] should be > 1') } 
		if (yfact < 2) { stop('fact[2] should be > 1') }
	} else {
		stop('length(fact) should be 1 or 2')
	}
	
	outraster <- raster(raster, filename)
	dataType(outraster) <- datatype
	outraster <- setRowCol(outraster, nrow(raster) * yfact, ncol(raster) * xfact) 

	if ( dataContent(raster)=='all') {
		
		cols <- rep(rep(1:ncol(raster), each=xfact), times=nrow(raster)*yfact)
		rows <- rep(1:nrow(raster), each=ncol(raster)*xfact*yfact)
		cells <- cellFromRowCol(raster, rows, cols)
		outraster <- setValues(outraster, values(raster)[cells])
		if (outraster@file@name != "") {
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
		
	} else if ( dataSource(raster) == 'disk') { 
		
		if (!canProcessInMemory(raster, 2 + xfact * yfact) && filename == '') {
			filename <- tempfile()
			filename(outraster) <- filename
			if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
		}
	
		starttime <- proc.time()
		
		v <- vector(length=0)
		cols <- rep(1:ncol(raster), each=xfact)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			for (i in 1:yfact) {
			
				if (outraster@file@name == "") {
					v <- c(v, values(raster)[cols])
				} else {
					outraster <- setValues(outraster, values(raster)[cols], (r-1) * xfact + i)
					outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
				}	
			}	
		}
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}

		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
	} 
	return(outraster)
}
