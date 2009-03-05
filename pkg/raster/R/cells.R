# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


cells <- function(x, ...) {
	funs <- list(...)
	if (length(funs) == 0) {
		return(NULL)
	}
	res <- list()
	if (dataContent(x) != 'all') {
		if (dataSource(x) == 'ram') {
			stop('no values associated with this RasterLayer')
		}
		if (.CanProcessInMemory(x, 2)) {
			x <- readAll(x)
		}
	}
	if (dataContent(x) == 'all') {
		for(i in seq(along=funs)) {
			res[i] <- funs[[i]](x@data@values)
		}
	} else {
		stop('sorry, only implemented for rasters that can be loaded into memory')
	}
}
	
