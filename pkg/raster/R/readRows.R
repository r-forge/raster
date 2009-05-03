# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.8
# Licence GPL v3


#read a block of data  (a rectangular area  of any dimension)  

.rasterReadRows <- function(raster, startrow, nrows=3) {
	if (startrow < 1 ) { stop("startrow too small") } 
	if (nrows < 1) { stop("nrows should be >= 1") } 
	
	endrow <- startrow+nrows-1
	if (endrow > nrow(raster)) {
		if (getOption('verbose')) { 
			warning("Rows beyond end of raster not read") 
		}
		endrow <- nrow(raster)
		nrows <- endrow - startrow + 1
	}
	
	nc <- raster@ncols * nrows
	
	raster <- .rasterRead(raster, startrow, 1, nc) 
	
	raster@data@content <- 'rows' 
	firstcell <- cellFromRowCol(raster, startrow, 1)
	lastcell <- cellFromRowCol(raster, endrow, ncol(raster))
	raster@data@indices <- c(firstcell, lastcell)

	return(raster)
}

