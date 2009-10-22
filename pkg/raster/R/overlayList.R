# Author: Robert J. Hijmans
# Contributors: Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.overlayList <- function(x, fun, filename="", ...){ 
	
	if (length(x) < 1) { stop('no RasterLayers') }
	compare(x)

	outraster <- raster(x[[1]], filename)
	
	.setDataType(outraster) <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

	inram <- TRUE
	for (i in 1:length(x)) {
		if (dataContent(x[[i]]) != 'all') {
			inram <- FALSE
		} 
	}	
	if (!canProcessInMemory(outraster,2)) {inram <- FALSE }

# what kind of function is this... 
# I must be overlooking a simpler approach here	
	a <- rep(1,10)
	tr <- try ( vals <- do.call(fun, list(a)), silent=TRUE )
	if (class(tr) == "try-error") {
		applymethod = FALSE
		vlist <- list()
		for (i in 1:length(x)) {
			vlist[[i]] <- a
		}
		tr <- try ( vals <- do.call(fun, vlist), silent=TRUE ) 
		if (class(tr) == "try-error") {
			stop('cannot use this formula')
		} 
		if (length(vals) != length(a)) {
			stop('cannot use this formula; lenghts do not match')	
		}
		
	} else {
		if (length(vals) == 1 && ncol(outraster) > 1) {
			m <- matrix(rep(a,length(x)), ncol=length(x), nrow=length(a))
			vals <- apply(m, 1, fun)
			if (length(vals) == length(a)) {
				applymethod = TRUE
			} else {
				stop('cannot use this formula')
			}
		}
	}

	vallist <- list()

	if ( inram ) {
		if (applymethod) {
			valmat <- vector()
			for (i in 1:length(x)) {
				valmat <- cbind(valmat, values(x[[i]]))
				x[[i]] <- clearValues(x[[i]])
			}	
			vals <- apply(valmat, 1, fun)
		} else {
			for (i in 1:length(x)) {
				vallist[[i]] <- values(x[[i]])
				x[[i]] <- clearValues(x[[i]])
			}
			vals <- do.call(fun, vallist)
		}
		
		outraster <- setValues(outraster, vals)
		if (outraster@file@name != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		if (outraster@file@name == "") {
			if (!canProcessInMemory(outraster, 4)) {
				filename <- rasterTmpFile()
				.setFilename(outraster) <- filename
			} else {
				v  <- vector(length=ncell(outraster))
				startcells <- cellFromCol(outraster, 1)
				endcells <- cellFromCol(outraster, ncol(outraster))
			}
		}	

		
		pb <- pbCreate(nrow(outraster), type=.progress(...))
		
		for (r in 1:nrow(outraster)) {
	
			if (applymethod) {
				valmat <- vector()
				for (i in 1:length(x)) {
					valmat <- cbind(valmat, getValues(x[[i]], r))
				}	
				vals <- apply(valmat, 1, fun)
			} else {
				for (i in 1:length(x)) {
					vallist[[i]] <- getValues(x[[i]], r)
				}	
				vals <- do.call(fun, vallist)
			}
			
			
			if (outraster@file@name == "") {
#				v <- c(v, vals)
				v[startcells[r]:endcells[r]] <- vals
			} else {
				outraster <- setValues(outraster, vals, r)
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}	
			
			pbStep(pb, r)
		}
		pbClose(pb)
		
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}
