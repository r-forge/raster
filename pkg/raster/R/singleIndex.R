# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("[", c("RasterLayer","ANY", "missing"),
function(x,i,j,...,drop=TRUE) {

	if (!missing(i) && class(i) == "RasterLayer") {
		i <- as.logical( .getRasterValues(i) ) 
	}

	if (dataContent(x) != 'all') {
		if (dataSource(x) != 'disk') {
			stop('no data associated with this RasterLayer object')
		} else {
			if (canProcessInMemory(x, 1)) {
				x <- readAll(x)
			}
		}
	}
	
	if (dataContent(x) == 'all') {
		values(x)[i, drop=drop]
	} else {
		if (missing(i)) {
			stop('raster too large.')
		} else {
			print('hello')
			return(cellValues(x, i))
		}
	}
}
)


