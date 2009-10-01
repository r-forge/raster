# Author: Robert J. Hijmans
# Contributors: Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.overlayList <- function(x, fun=sum, filename="", ...){ 
	
	compare(x)

	outraster <- raster(x[[1]], filename)
	
	dataType(outraster) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

	inram <- TRUE
	for (i in 1:length(x)) {
		if (dataContent(x[[i]]) != 'all') {
			inram <- FALSE
		} 
	}	
	if (!canProcessInMemory(outraster,2)) {inram <- FALSE }
	
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
		pb <- .setProgressBar(nrow(outraster), type=.progress(...))

		for (r in 1:nrow(outraster)) {
			for (i in 1:length(x)) {
				vallist[[i]] <- getValues(x[[i]], r)
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
			
			.doProgressBar(pb, r)
		}
		.closeProgressBar(pb, starttime)
		
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}
