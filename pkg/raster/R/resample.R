# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  Jaunary 2009
# Version 0.8
# Licence GPL v3



resample <- function(from, to, method="ngb", filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1)  {
	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') 	}
		
	bb <- intersectBbox(from, to)
	validObject(bb)
	if (is.null(filename)){filename <- ""}
	to <- raster(to, filename)
	to <- setDatatype(to, datatype)
	
	if (!canProcessInMemory(to, 1) && filename(to) == '') {
		filename <- tempfile()
		to <- setFilename(to, filename )
		if (options('verbose')[[1]]) { cat('writing raster to:', filename(to))	}
	}
	inMemory <- filename(to) == ""

	v <- vector(length=0)
	rowCells <- 1:ncol(to)
	starttime <- proc.time()

	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		if (method=='ngb') {
			vals <- xyValues(from, xy)
		} else {
			vals <- xyValues(from, xy, method='bilinear')
		}
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite, filetype=filetype)
		}

		if (r %in% track) { .showTrack(r, to@nrows, track, starttime) }
		
	}
	if (inMemory) {
		to <- setValues(to, v) 
	}
	return(to)
}

