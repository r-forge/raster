# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


#read a single row of data

.rasterReadRow <- function(raster, r) {
	if (row < 1 ) { stop("startrow too small") } 
	if (row > raster@nrows ) { stop("row should be <= nrow(raster)") } 
	raster <- .rasterRead(raster, r)
	raster@data@content <- 'row' 
	firstcell <- cellFromRowCol(raster, row, 1)
	lastcell <- cellFromRowCol(raster, row, raster@ncols)
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}

