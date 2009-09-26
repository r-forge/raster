# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3

.addToList <- function(x, r) {
	if (class(r) == 'character') {
		r <- raster(r)
		# or r <- unstack(stack(r, -1)) ???
		return( c(x, r) )
	} else if (! extends(class(r), 'Raster')) {
		stop('... arguments must be a filename or objects that extend the Raster class')
	} else if (class(r) == 'RasterLayer') {
		return( c(x, r) )
	} else {
		return( c(x, unstack(r)) )
	} 
}


.makeRasterList <- function(...) {
	arg <- list(...)
	x <- list()
	for (i in seq(along=arg)) {
		if (class(arg[[i]]) == 'list') {
			for (j in 1:length(arg[[i]])) {
				x <- .addToList(x, arg[[i]][[j]]) 
			}
		} else {
			x <- .addToList(x, arg[[i]]) 
		}
	}
	return(x)
}

			