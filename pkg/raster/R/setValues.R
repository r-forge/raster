# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


setValuesRows <- function(object, values, startrow, endrow) {
	if (!is.vector(values)) {stop('values must be a vector')}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	}
	startow <- round(start)
	endrow <- round(endrow)
	if (startrow > endrow) {stop()}
	if (startrow < 1 | startrow > nrow(object)) {
		stop(paste("rownumber out of bounds"))
	}
	object@data@values <- values
	object@data@content <- 'row' 
	firstcell <- cellFromRowCol(object, rownr=startrow, colnr=1)
	lastcell <- cellFromRowCol(object, rownr=endrow, colnr=ncol(object))
	object@data@indices <- c(firstcell, lastcell)
	if (length(values) != (lastcell-firstcell) + 1) {stop('wrong length')}
	return(object)
}


if (!isGeneric('setValues')) {
	setGeneric('setValues', function(object, values, rownr=-1, layer=-1)
		standardGeneric('setValues')) 
	}	

		
setMethod('setValues', signature(object='RasterLayer'), 
  
function(object, values, rownr=-1, layer=-1) {
  
	if (!is.vector(values)) {stop('values must be a vector')}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	}
	

	if (length(values) == 1) {	
		if (rownr > 0) { 
			values <- rep(values, ncol(object))
		} else {
			values <- rep(values, ncell(object))
		}
	}

	if (length(values) == ncell(object)) { 
		if (rownr > 0) {
			stop("if setting all values, rownr must be < 1")
		}
		object@data@values <- values
		object@data@content <- 'all'
		object@data@source <- 'ram'
		object@data@indices <- c(1, ncell(object))
		object <- setMinMax(object)
		return(object)	
	} else if (length(values) == ncol(object)) {
		rownr <- round(rownr)
		if (rownr < 1 | rownr > nrow(object)) {
			stop(paste("rownumber out of bounds:", rownr))
		}
		object@data@values <- values
		object@data@content <- 'row' 
		firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
		lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
		object@data@indices <- c(firstcell, lastcell)
		return(object)
	} else if (length(values) / ncol(object) ) {
		
	} else {
		stop("length(values) is not equal to ncell(object) or ncol(object)") 
	}
 }
)
	


setMethod('setValues', signature(object='RasterStack'), 
  function(object, values, rownr=-1, layer=-1) {
	if (!(is.vector(values) | is.matrix(values))) {
		stop('values must be a vector or a matrix')
	}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}
	rownr <- round(rownr)
	
	if (is.matrix(values)) {
		if (ncol(values) == nlayers(object)) {
			object@data@values <- values
			if (nrow(values) == 1) {
				object@data@content <= 'all'
				object@data@indices <- c(1, ncell(object))
			} else if (nrow(values) == nrow(object)) {
				object@data@content <= 'row'
				firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
				lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
				object@data@indices <- c(firstcell, lastcell)				
			} else {
				stop('either set all data or a single row')
			}
		} else if (ncol(values) == 1) {
			values <- as.vector(values)
		} else {
			stop('either set values for all layers or for a single layer')
		}
	}
	
	if (is.vector(values)) {
		layer <- round(layer)
		if (layer < 1) { 
			print(class(object))
			stop('specify layer')	}
		if (layer > nlayers(object)) {stop('layer number too high')}
		
		
		if (length(values) == ncell(object)) { 
			if (rownr > 0) {
				stop("if setting all values, rownr must be < 1")
			}
			if (dataContent(object) != 'all') { 
				stop(" you can only setValues with these values if the dataContent = 'all'") }
			object@data@values[,layer] <- values
	#		object <- setMinMax(object)
		} else if (length(values) == ncol(object)) {
			if (rownr < 1 | rownr > nrow(object)) {
				stop(paste("rownumber out of bounds:", rownr))
			}
			object@data@values <- values
			object@data@content <- 'row' 
			firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
			lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
			object@data@indices <- c(firstcell, lastcell)
		}
	} else {
		stop("length(values) is not equal to ncell(object) or ncol(object)") 
	}
 }
)
	
	
	

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
	