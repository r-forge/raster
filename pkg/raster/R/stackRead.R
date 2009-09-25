# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3


.stackRead <- function(rstack, rownumber, startcol=1, ncolumns=(ncol(rstack)-startcol+1)) {
	for (i in seq(nlayers(rstack))) {
		if (dataSource(rstack@layers[[i]]) == 'disk') {
			r <- .rasterRead(rstack@layers[[i]], rownumber, startcol, ncolumns)
		}
	}
	return(rstack)
}




.stackReadCells <- function(object, cells) {
		for (i in seq(nlayers(object))) {
			v <- .readCells( raster(object, i), cells)
			if (i == 1) {
				result <- v
			} else {
				result <- cbind(result, v)
	#			colnames(result)[length(result[1,])] <- rstack@layers[[i]]@layernames
			}
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- layerNames(object)
		}	
		return(result)
}


