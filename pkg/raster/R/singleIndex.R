# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[", c("RasterLayer","ANY", "missing"),
function(x,i,j,...,drop=TRUE) {
	
	if (dataContent(x) != 'all') {
		if (dataSource(x) != 'disk') {
			stop('no data associated with this RasterLayer object')
		}
	}
	
	if (missing(i)) {
		if (dataContent(x) == 'all') {
			return(values(x))
		} else {
			return(values(readAll(x)))
		}
	}

	if (class(i) == "RasterLayer") {
		i <- as.logical( getValues(i) ) 
	}
	
	if (dataContent(x) != 'all') {
		if (canProcessInMemory(x, 2)) {
			if (length(i) > 0.5 * ncell(x)) {
				x <- readAll(x)
			}
		}
	}
	
	if (dataContent(x) == 'all') {
		values(x)[i, drop=drop]
	} else {
		return(cellValues(x, i))
	}
}
)


