# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3



setReplaceMethod("[", c("RasterLayer", "RasterLayer"),
	function(x, i, j, value) {

		if (! missing(j) ) { 
			stop('if the first index is a RasterLayer object, there cannot be a second index') 
		}	
	
		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
			
		} else if (compare(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			i <- as.logical( getValues(i) )
			i[is.na(i)] <- FALSE
			
		} else {
			i <- cellsFromExtent(x, i)
		}		
	
		return(callNextMethod(x, i, j, value=value))
	}
)


setReplaceMethod("[", c("RasterLayer", "Extent"),
	function(x, i, j, value) {

		if (! missing(j) ) { 
			stop('if the first index is an Extent oject, there cannot be a second index') 
		}	
		
		i <- cellsFromExtent(x, i)
		return(callNextMethod(x, i, j, value=value))
	}
)

setReplaceMethod("[", c("RasterLayer", "Spatial"),
	function(x, i, j, value) {

		if (! missing(j) ) { 
			stop('if the first index is a Spatial* object, there cannot be a second index') 
		}	
	
		if (inherits(i, 'SpatialPolygons')) {
			v <- 1:length(i@polygons)
			v[] <- value
			return( .polygonsToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else if (inherits(i, 'SpatialLines')) {
			v <- 1:length(i@lines)
			v[] <- value
			return( .linesToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else { # if (inherits(i, 'SpatialPoints')) {
			i <- cellsFromXY(x, coordinates(i))
			return( callNextMethod(x, i, j, value=value) )
		}
	}
)



setReplaceMethod("[", c("RasterLayer","ANY"),
	function(x, i, j, value) {

	if (! missing(j) ) { 
		if (! is.numeric(j)) { 
			stop('the second argument must be numeric (or missing)') 
		}	
		if (! missing(i)) {
			if (! is.numeric(i)) {
				stop('the first index must be numeric if you supply a second index') 
			}
		}
	} 

	if (! is.numeric(value) & !is.logical(value)) { 
		value <- as.numeric(value) 
	}
		
	if ( missing(i) ) {
	
		if (missing(j)) {
			# all cells
			
			if (length(value) == ncell(x)) {
				x <- try( setValues(x, value))
			} else if (length(value) == 1) {
				x <- try( setValues(x, rep(value, times=ncell(x))) )
			} else {
				v <- try( vector(length=ncell(x)) )
				if (class(x) != 'try-error') {
					v[] <- value
					x <- try( setValues(x, v) )
				}
			}
			if (class(x) == 'try-error') {
				stop('cannot replace values on this raster (it is too large')
			}
			return(x)
			
		} else {
			# columns
			i <- cellFromCol(x, j)
		} 
		
	} else {

		if (! (is.numeric(i) | is.logical(i)) ) {
			stop('the first index must be numeric or logical') 
		}
	
		if (missing(j)) {
			theCall <- sys.call(-1)
			narg <- length(theCall)-length(match.call(call=sys.call(-1)))
			if (narg > 0) {
				i <- cellFromRow(x, i)
			}
		} else {
			i <- cellFromRowColCombine(x, i, j)
		}
	}

	if ( is.logical(i) ) {
		i[is.na(i)] <- FALSE
	} else {
		i <- na.omit(i)
		i <- subset(i, i >= 1 & i <= ncell(x))
	}
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			x <- try( readAll(x) )
		} else {
			x <- try (setValues(x, rep(NA, times=ncell(x))) )
		}
		if (class(x) == 'try-error') {
			stop('cannot do in-memory replace values on this raster (it is too large)')
		}
	}
		
		
	x@data@values[i] <- value
	x <- setMinMax(x)
	x <- .clearFile(x)
	return(x)
}
)

