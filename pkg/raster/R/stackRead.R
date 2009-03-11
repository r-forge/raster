# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


.stackRead <- function(rstack, rownumber, startcol=1, ncolumns=(ncol(rstack)-startcol+1)) {
	if (dataContent(rstack) == 'all') {  
		if (rownumber > 0) {
			warning('all values are in memory; no point in using read')
		}
		return(rstack)
	}

	for (i in seq(nlayers(rstack))) {
		r <- .rasterRead(rstack@layers[[i]], rownumber, startcol, ncolumns)
		if (i==1) {
			rstack@data@values <- matrix(nrow=length(values(r)), ncol=nlayers(rstack)) 
			rstack@data@content <- dataContent(r)
			rstack@data@indices <- dataIndices(r)
		}
		rstack@data@values[,i] <- values(r)
	}
	return(rstack)
}



.stackReadXY <- function(object, xyCoords, method='simple') {

	if (method == 'bilinear') {
		for (i in seq(nlayers(object))) {
			v <- .bilinearValue(object, xyCoords)
			if (i == 1) {
				result <- v
			} else {
				result <- cbind(result, v)
			}
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- object@data@colnames
		}	
		return(result)		
	
	} else {
		cells <- cellFromXY(object, xyCoords)
		return(.stackReadCells(object, cells))
	}
	
}


.stackReadCells <- function(object, cells) {
		for (i in seq(nlayers(object))) {
			v <- .rasterReadCells(object@layers[[i]], cells)
			if (i == 1) {
				result <- v
			} else {
				result <- cbind(result, v)
	#			colnames(result)[length(result[1,])] <- rstack@layers[[i]]@file@shortname
			}
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- object@data@colnames
		}	
		return(result)
}

