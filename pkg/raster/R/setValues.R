# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setValuesRows <- function(object, values, firstcell, lastcell) {
	if (!is.vector(values)) {stop('values must be a vector')}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}
	if (firstcell > lastcell) {stop()}
	if (firstcell < 1 | lastcell > ncell(object)) {
		stop(paste("indices out of bounds"))
	}
	object@data@values <- values
	object@data@content <- 'rows' 
	object@data@indices <- c(firstcell, lastcell)
	if (length(values) != (lastcell-firstcell + 1)) {stop('wrong length')}
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
		if (rownr > 0 & nrow(object) > 1) {
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
	} else {
		stop("length(values) is not equal to ncell(object), or to ncol(object), or 1") 
	}
 }
)
	


setMethod('setValues', signature(object='RasterBrick'), 
  function(object, values, rownr=-1, layer=-1) {
	if (!(is.vector(values) | is.matrix(values))) {
		stop('values must be a vector or a matrix')
	}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}
	rownr <- round(rownr)
	
	if (layer < 1) {
		if (!is.matrix(values)) {
			values <- matrix(values)
		}
		if (nrow(values) == ncell(object)) {
			object@data@nlayers <- ncol(values)
			object@data@content <- 'all'
			object@data@indices <- c(1, ncell(object))
			object@data@values <- values
			object <- setMinMax(object)
		} else if (nrow(values) == ncol(object)) {
			if (object@data@nlayers != ncol(values)) {
				if (rownr==1) {
					object@data@nlayers <- ncol(values)
				} else {
					stop('ncol does not match nlayers' )
				}
			}	
			object@data@content <- 'row'
			firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
			lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
			object@data@indices <- c(firstcell, lastcell)				
			object@data@values <- values
		} else {
			stop('either set all data or a single row')
		}
	} else {
		if (nlayers(object)==0) {object@data@nlayers <- 1 }
		layer <- round(layer)
		if (layer > nlayers(object)) {stop('layer number too high')}
		
		if (length(values) == ncell(object) & nrow(object) > 1) { 
			if (rownr > 0) {
				stop("if setting all values, rownr must be < 1")
			}
			if (dataContent(object) != 'all') { 
				atry <- try(object <- readAll(object), silent=T)
				if (class(atry) == "try-error") {
					stop(" you can only setValues for a single layer if all values are in memory. But values could not be loaded")				
				}
			}
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
			if (object@data@indices != c(firstcell, lastcell)) {
				stop('setting values for the wrong row number')
			}
		} else {
			stop("length(values) is not equal to ncell(object) or ncol(object)") 
		}
	}
	return(object)
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
	