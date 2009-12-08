# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


#read a block of data  (a rectangular area  of any dimension)  
.rasterReadBlock <- function(raster, startrow, nrows=3, startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	startrow <- round(startrow)
	startcol <- round(startcol)
	nrows <- round(nrows)
	ncolums <- round(ncolumns)
	if (startrow < 1 ) { stop("startrow too small") } 
	if (nrows < 1) { stop("nrows should be >= 1") } 
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)") }
	if (ncolumns > ncol(raster)) { ncolumns <- ncol(raster)-startcol+1 } 
	endcol <- startcol + ncolumns-1
	endrow <- startrow + nrows-1
	if (endrow > nrow(raster)) {
		if (getOption('verbose')) { 
			warning("Rows beyond end of raster not read") 
		}
		endrow <- nrow(raster)
		nrows <- endrow - startrow + 1
	}

	if (dataSource(raster)=='ram') {
		if (dataContent(raster) != 'all') {
			stop('cannot read data for this RasterLayer')
		} 
	}
	if (dataContent(raster) == 'all') {
		block <- t( values(raster, format='matrix')[startrow:endrow, startcol:endcol] )
	} else {
		block <- matrix(ncol=nrows, nrow=ncolumns)
		for (r in 1:nrows) {
			block[,r] <- values( .rasterRead(raster, r-1+startrow,  startcol, ncolumns) )
		}
	}
	raster@data@values <- as.vector(block)
	raster@data@content <- 'block' 
	firstcell <- cellFromRowCol(raster, startrow, startcol)
	lastcell <- cellFromRowCol(raster, endrow, (startcol+ncolumns-1))
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}

