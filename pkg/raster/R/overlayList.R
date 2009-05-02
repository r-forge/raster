# Author: Robert J. Hijmans and Reinhard Krug
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.overlayList <- function(x, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	
	compare(x)

	outraster <- raster(x[[1]], filename)
	dataType(outraster) <- datatype

	inram <- TRUE
	for (i in 1:length(x)) {
		if (dataContent(x[[i]]) != 'all') {
			inram <- FALSE
		} 
	}	
	
	vallist <- list()

	if ( inram ) {
		for (i in 1:length(x)) {
			vallist[[i]] <- values(x[[i]])
			clearValues(x[[i]])
		}
		vals <- do.call(fun, vallist)
		
		outraster <- setValues(outraster, vals)
		if (outraster@file@name != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		if (outraster@file@name == "") {
			if (!canProcessInMemory(outraster, 4)) {
				filename <- rasterTmpFile()
				filename(outraster) <- filename
			} else {
				v  <- vector(length=ncell(outraster))
				startcells <- cellFromCol(outraster, 1)
				endcells <- cellFromCol(outraster, ncol(outraster))
			}
		}	
		starttime <- proc.time()

		for (r in 1:nrow(outraster)) {
			for (i in 1:length(x)) {
				if (dataSource(x[[i]]) == 'ram') {
					x[i] <- valuesRow(x[[i]], r)
				} else {	
					x[i] <- readRow(x[[i]], r)
				}	
			}	
			
			for (i in 1:length(x)) {
				vallist[[i]] <- values(x[[i]])
			#	clearValues(rasters[[i]])
			}
			vals <- do.call(fun, vallist)
			
			if (r == 1) {
				if (length(vals) == 1 && ncol(outraster) > 1) {
					stop('single value returned for a row; inappropriate formula used')
				}
			}
			
			if (outraster@file@name == "") {
#				v <- c(v, vals)
				v[startcells[r]:endcells[r]] <- vals
			} else {
				outraster <- setValues(outraster, vals, r)
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}	
			
			if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
			
		}
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}
