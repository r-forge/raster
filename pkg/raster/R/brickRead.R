# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3


.brickRead <- function(brick, rownumber, startcol=1, ncolumns=(ncol(brick)-startcol+1)) {
	if (dataContent(brick) == 'all') {  
		if (rownumber > 0) {
			warning('all values are in memory; no point in using read')
		}
		return(brick)
	}

	for (i in seq(nlayers(brick))) {
		r <- raster(filename(brick), i)
		r <- .rasterRead(r, rownumber, startcol, ncolumns)
		if (i==1) {
			brick@data@values <- matrix(nrow=length(values(r)), ncol=nlayers(brick)) 
			brick@data@content <- dataContent(r)
			brick@data@indices <- dataIndices(r)
		}
		brick@data@values[,i] <- values(r)
	}
	
	if (dataContent(brick) == 'all') {
		brick <- setMinMax(brick)
	}	
	
	return(brick)
}




.brickReadCells <- function(object, cells) {
		for (i in seq(nlayers(object))) {
			r <- raster(filename(brick), i)
			if (dataContent(brick) == 'all') {
				r[] <- object@data@values[,i]
			}
			v <- .rasterReadCells( r, cells)
			if (i == 1) {
				result <- v
			} else {
				result <- cbind(result, v)
	#			colnames(result)[length(result[1,])] <- brick@layers[[i]]@file@shortname
			}
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- object@data@colnames
		}	
		return(result)
}


