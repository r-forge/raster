# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setMethod("[[", c("RasterLayer","ANY", "ANY"),
function(x,i,j,...,drop=TRUE) {

	if (!missing(i) && class(i) == "RasterLayer") {
		i <- as.logical( .getRasterValues(i) ) 
	}

	if (dataContent(x) != 'all') {
		if (dataSource(x) != 'disk') {
			stop('no data associated with this RasterLayer object')
		} else {
			if (.CanProcessInMemory(x, 1)) {
				x <- readAll(x)
			}
		}
	}

	if (dataContent(x) == 'all') {
		m <- matrix(values(x), nrow(x), ncol(x), byrow=T)
		rm(x)
#		callNextMethod(m, i=i, j=j, drop=drop)
		return(m[i=i, j=j, drop=drop])
	} else {
		if ( missing(j) ) {
			#argsn <- nargs() - length(list(...)) - !missing(drop)
			#if ( argsn == 2 ) {
			#	return(cellValues(x, i))
			#} else {
				cells <- cellFromRow(x, i)
				return(cellValues(x, cells))
			#} 
		} else if (missing(i)) {
			cells <- cellFromCol(x, j)
			return(cellValues(x, cells))
		} else {
		# bound to fail in most cases:
			cells <- cellFromRowColCombine(x, i, j)
			return(cellValues(x, cells))
		}

	}
}
)

