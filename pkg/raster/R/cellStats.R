# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3

.Old.cellStats <- function(x, ..., na.rm=TRUE) {
	funs <- list(...)
	if (length(funs) == 0) {
		stop('you must provide a function as argument')
	}
	res <- list()
	if (dataContent(x) != 'all') {
		if (dataSource(x) == 'ram') {
			stop('no values associated with this RasterLayer')
		}
		if (canProcessInMemory(x, 2)) {
			x <- readAll(x)
		}
	}
	if (dataContent(x) == 'all') {
		for(i in seq(along=funs)) {
			if (na.rm) {
				res[i] <- funs[[i]](na.omit(x@data@values))
			} else {
				res[i] <- funs[[i]](x@data@values)
			}
		}
	} else {
		stop('sorry, only implemented for rasters that can be loaded into memory')
	}
	return(unlist(res))
}
	
