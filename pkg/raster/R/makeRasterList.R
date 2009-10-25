# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3

.addToList <- function(x, r, giveError=FALSE) {
	if (class(r) == 'character') {
		r <- raster(r)
		# or r <- unstack(stack(r, -1)) ???
		return( c(x, r) )
	} else if (! extends(class(r), 'Raster')) {
		if (giveError) {
			stop('... arguments must be a filename or objects that extend the Raster class')
		} else {
			return(x)
		}
	} else if (class(r) == 'RasterLayer') {
		return( c(x, r) )	
	} else {
		return( c(x, unstack(r)) )
	} 
}



.makeRasterList <- function(..., giveError=FALSE, keepone=FALSE) {
	arg <- list(...)
	x <- list()
	for (i in seq(along=arg)) {
		if (class(arg[[i]]) == 'list') {
			for (j in 1:length(arg[[i]])) {
				x <- .addToList(x, arg[[i]][[j]], giveError) 
			}
		} else {
			x <- .addToList(x, arg[[i]], giveError) 
		}
	}
	for (i in rev(seq(along=x))) {
		if (dataContent(x[[i]]) != 'all'  &  dataSource(x[[i]]) == 'ram' ) {
			if (length(x) > 1 | keepone==FALSE ) {
				x <- x[[-i]]
				warning('RasterLayer with no data ignored')
			} 
		} 
	}		
	return(x)
}

