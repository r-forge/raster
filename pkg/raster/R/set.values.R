# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3



setValues <- function(raster, values, rownr=-1) {
	if (!is.vector(values)) {stop('values must be a vector')}
	if (length(values) == 0) {	stop('length(values==0). If this is intended then use clearValues(raster)') }
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {stop('data must be values')}
	rownr <- round(rownr)
	if (length(values) == ncells(raster)) { 
		if (rownr > 0) {
			stop("if setting all values, rownr must be < 1")
		}
		raster@data@values <- values
		raster@data@content <- 'all'
		raster@data@source <- 'ram'
		raster@data@indices <- c(1, ncells(raster))
		raster <- setMinMax(raster)
		return(raster)	
	} else if (length(values) == ncol(raster)) {
		if (rownr < 1 | rownr > nrow(raster)) {
			stop(paste("rownumber out of bounds:", rownr))
		}
		raster@data@values <- values
		raster@data@content <- 'row' 
		firstcell <- cellFromRowcol(raster, rownr=rownr, colnr=1)
		lastcell <- cellFromRowcol(raster, rownr=rownr, colnr=ncol(raster))
		raster@data@indices <- c(firstcell, lastcell)
		return(raster)
	} else {
		stop("length(values) is not equal to ncells(raster) or ncol(raster)") 
	}
}	
	


clearValues <- function(object) {
	object@data@content <- 'nodata'
	object@data@indices <- ""
	if (class(object) == 'RasterLayer') {
		object@data@values <- vector()
	} else {
		object@data@values <- matrix(NA,0,0)
	}
	return(object)
}



makeSparse <- function(raster) {
	if ( dataContent(raster) == 'sparse') {return(raster)
	} else {
		if ( dataContent(raster) == 'all') {
			vals <- seq(1:ncells(raster))
			vals <- cbind(vals, values(raster))
			vals <- as.vector(na.omit(vals))
			raster <- setValuesSparse(raster, sparsevalues=vals[,2], cellnumbers=vals[,1])
			return(raster)
		} else { 
			# as above, but by reading data from disk, row by row
			stop('not implemented yet, use readAll() first' )
		}	
	}
}

setValuesSparse <- function(raster, sparsevalues, cellnumbers) {
	if (!(isTRUE(length(cellnumbers) == (length(sparsevalues))))) {
		stop()
	}
	raster@data@content <- 'sparse'
	raster@data@values <- sparsevalues
	raster@data@indices <- cellnumbers
	raster@data@source <- 'ram'
	raster <- setMinMax(raster)
	return(raster)
}

setValuesBlock <- function(raster, blockvalues, firstcell, lastcell) {
	if (!is.vector(blockvalues)) {	stop('values must be a vector') }
	if (length(blockvalues) == 0) {	stop('length(blockvalues==0). If this is intended use raster.data.clear(raster)') }
	if (!(is.numeric(blockvalues) | is.integer(blockvalues) | is.logical(blockvalues))) { stop('values must be numeric, integer or logical') }
	
	firstcol <- colFromCell(raster, firstcell)
	lastcol <- colFromCell(raster, lastcell)
	firstrow <- rowFromCell(raster, firstcell)
	lastrow <- rowFromCell(raster, lastcell)
	ncells <- (lastcol - firstcol + 1) * (lastrow - firstrow + 1)
	
	if (ncells != length(blockvalues)) { 
		stop( paste("length(blockdata):", length(blockvalues), "does not match the number implied by firstcell and lastcell:", ncells)) 
	}
	raster@data@values <- blockvalues
	raster@data@content <- 'block' 
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}

