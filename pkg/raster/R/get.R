# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3


.isSPgrid <- function(object){
	if (class(object) == "SpatialGrid" | class(object) == "SpatialGridDataFrame" | class(object) == "SpatialPixels" | class(object) == "SpatialPixelsDataFrame") {
		return(TRUE)
	} else {
		return(FALSE)
	}	
}
	
rowFromCell <- function(object, cell) {
	if (.isSPgrid(object)) { object <- raster(object) }
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
    return(rownr)
}


cellFromRow <- function(object, rownr) {
	if (.isSPgrid(object)) { object <- raster(object) }
	cols <- rep(1:ncol(object), times=length(rownr))
	rows <- rep(rownr, each=length(cols))
	return(cellFromRowCol(object, rows, cols))
}

cellFromCol <- function(object, colnr) {
	if (.isSPgrid(object)) { object <- raster(object) }
	rows <- rep(1:nrow(object), times=length(colnr))
	cols <- rep(colnr, each=nrow(object))
	return(cellFromRowCol(object, rows, cols))
}

cellFromRowColCombine <- function(object, rownr, colnr) {
	rows <- cellFromRow(object, rownr)
	cols <- cellFromCol(object, colnr)
	return(intersect(rows, cols))
}

colFromCell <- function(object, cell) {
	if (.isSPgrid(object)) { object <- raster(object) }
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA	
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
	colnr <- as.integer(cell - ((rownr-1) * ncol(object)))
    return(colnr)
}

cellFromRowCol <- function(object, rownr, colnr) {
	if (.isSPgrid(object)) { object <- raster(object) }
	rownr <- round(rownr)
	colnr <- round(colnr)
	rownr[rownr < 1 | rownr > nrow(object)] <- NA
	colnr[colnr < 1 | colnr > ncol(object)] <- NA	
	return((rownr-1) * ncol(object) + colnr)
}

