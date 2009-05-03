# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.8
# Licence GPL v3


#read a block of data  (a rectangular area  of any dimension)  
.rasterReadBlock <- function(raster, startrow, nrows=3, startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
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

	blockvalues <- vector()
	for (r in (startrow:endrow)) {
		raster <- .rasterRead(raster, r,  startcol, ncolumns)
		blockvalues <- c(blockvalues, values(raster))
	}

	raster@data@values <- blockvalues
	raster@data@content <- 'block' 
	firstcell <- cellFromRowCol(raster, startrow, startcol)
	lastcell <- cellFromRowCol(raster, endrow, (startcol+ncolumns-1))
	raster@data@indices <- c(firstcell, lastcell)

	return(raster)
}

