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
	filename <- trim(filename)
	outraster <- raster(raster)
	dataType(outraster) <- datatype
	rowcol(outraster) <- c(nrow(raster) * yfact, ncol(raster) * xfact) 

	if (dataContent(raster) == 'nodata' & dataSource(raster) == 'ram') {
		return(outraster)
	}
	
	if (!canProcessInMemory(outraster, 3) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}
	filename(outraster) <- filename
	
	if ( filename == "" ) {
		if (dataContent(raster) != 'all') {
			raster <- readAll(raster)
		}
		cols <- rep(rep(1:ncol(raster), each=xfact), times=nrow(raster)*yfact)
		rows <- rep(1:nrow(raster), each=ncol(raster)*xfact*yfact)
		cells <- cellFromRowCol(raster, rows, cols)
		outraster <- setValues(outraster, values(raster)[cells])
		
	} else { 
		# to speed up valuesRow
		if (dataContent(raster) != 'all') { raster <- clearValues(raster) }
		starttime <- proc.time()		
		v <- vector(length=0)
		cols <- rep(1:ncol(raster), each=xfact)
		for (r in 1:nrow(raster)) {
			vals <- valuesRow(raster, r)
			for (i in 1:yfact) {
				outraster <- setValues(outraster, vals[cols], (r-1) * xfact + i)
				outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
			}	
			if (r %in% track) { .showTrack(r, raster@nrows, track, starttime) }
		}
	} 
	return(outraster)
}
