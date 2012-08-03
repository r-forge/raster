# Authors: Robert J. Hijmans
# Date :  August 2009
# Version 1.0
# Licence GPL v3



if (!isGeneric('subset')) {
	setGeneric('subset', function(x, ...)
		standardGeneric('subset')) 
}


setMethod('subset', signature(x='RasterStack'), 
function(x, subset, drop=TRUE, ...) {
	if (is.character(subset)) {
		i <- na.omit(match(subset, names(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}
	subset <- as.integer(subset)
	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}
	if (length(subset) == 1 & drop) {
		x <- x@layers[[subset]]
	} else {
		x@layers <- x@layers[subset]
		x@layernames <- x@layernames[subset]
		if (length(x@z)>0) {
			x@z <- lapply(x@z, function(x) x[subset])
		}
	}
	return(x)	
} )


setMethod('subset', signature(x='RasterLayer'),
function(x, subset, ...) {
	return(x)
}
)

setMethod('subset', signature(x='RasterBrick'),
function(x, subset, drop=TRUE, ...) {

	if (is.character(subset)) {
		i <- na.omit(match(subset, names(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}
	
	subset <- as.integer(subset)
	nl <- nlayers(x)
	if (! all(subset %in% 1:nl)) {
		stop('not a valid subset')
	}
	if (nl==1) {
		return(x)
	}
	
	varname <- attr(x@data, "zvar")
	if (is.null(varname)) { 
		varname <- "" 
	}

	nav <- NAvalue(x)
	
	if (fromDisk(x)) {
		if (drop & length(subset)==1) {
			x <- raster(filename(x), band=subset, varname=varname)
		} else {
			x <- stack(filename(x), bands=subset, varname=varname)
		}
		NAvalue(x) <- nav
		return(x)
	} else {
		if (drop & length(subset)==1) {
			if (hasValues(x)) {
				x <- raster(x, subset)
			} else {
				x <- raster(x)
			}
			NAvalue(x) <- nav
			return(x)	
		}
	
		if (hasValues(x)) {
			x@data@values <- x@data@values[, subset, drop=FALSE]
			x@data@min <- x@data@min[subset]
			x@data@max <- x@data@max[subset]
		}	
		x@layernames <- x@layernames[subset]
		if (length(x@z) > 0) {
			x@z[[1]] <- x@z[[1]][subset]
		}
		x@data@nlayers <- as.integer(length(subset))
		return(x)
	}
} )


