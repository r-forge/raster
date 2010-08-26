# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
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
		subset = .nameToIndex(subset, layerNames(x))
	}
	
	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}
	if (length(subset) == 1 & drop) {
		x <- raster(x, subset)
	} else {
		x@layers <- x@layers[subset]
	}
	return(x)	
} )




setMethod('subset', signature(x='RasterBrick'), 
function(x, subset, drop=TRUE, ...) {

	if (is.character(subset)) {
		subset = .nameToIndex(subset, layerNames(x))
	}

	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}
	if (length(subset) == 1 & drop) {
		x <- raster(x, subset)
	} else {
		if (inMemory(x)) {
			if (length(unique(subset)) == length(subset)) {
				drops <- which(! 1:nlayers(x) %in% subset)
				x <- dropLayer(x, drops)
			} else {
				x@data@values <- x@data@values[, index, drop=FALSE]
				x@layernames <- x@layernames[index]
				x@data@nlayers <- ncol(x@data@layers)
				x@data@min <- x@data@min[index]
				x@data@max <- x@data@max[index]
			}
		} else {
			x <- stack(x)
			x <- subset(x, subset=subset, drop=drop, ...)
		}
	}
	return(x)	
} )

