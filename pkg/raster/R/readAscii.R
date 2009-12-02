# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2009
# Version 0.9
# Licence GPL v3


.getAsciiData <- function(x, rownr=-1, startcol=1, ncolumns=ncol(x)) {
	if (rownr > 0) {
		v <- .readRowAscii(x, rownr)
		endcol <- min(ncol(x), (startcol+ncolumns-1))
		v <- v[startcol:endcol]
	} else {
		return(.readAllAscii(x))
	}
}

.readAllAscii <- function(x) {
	filename <- trim(filename(x))
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	v <- as.numeric( scan(filename, skip=6, what='character', quiet=TRUE) )
	if (x@file@nodatavalue < 0) {
		v[v <= x@file@nodatavalue ] <- NA 			
	} else {
		v[v == x@file@nodatavalue ] <- NA 					
	}	
	return ( v ) 
}


.readRowAscii <- function(x, rownr) {
	filename <- trim(filename(x))
	skiprows <- 6 + rownr - 1 
	v <- as.numeric ( scan(filename, skip=skiprows, nlines=1, what='character', quiet=TRUE) )
	if (x@file@nodatavalue < 0) {
		v[v <= x@file@nodatavalue ] <- NA 			
	} else {
		v[v == x@file@nodatavalue ] <- NA 					
	}
	return ( v )
}


.readRowsAscii <- function(x, startrow, endrow) {
	filename <- trim(filename(x))
	skiprows <- 6 + startrow - 1 
	nrows <- endrow - startrow + 1
	v <- as.numeric ( scan(filename, skip=skiprows, nlines=nrows, what='character', quiet=TRUE) )
	if (x@file@nodatavalue < 0) {
		v[v <= x@file@nodatavalue ] <- NA 			
	} else {
		v[v == x@file@nodatavalue ] <- NA 					
	}
	return ( v )
}


.readCellsAscii <- function(raster, cells) {
	colrow <- matrix(ncol=5, nrow=length(cells))
	colrow <- matrix(ncol=5, nrow=length(cells))
	colrow[,1] <- colFromCell(raster, cells)
	colrow[,2] <- rowFromCell(raster, cells)
	colrow[,3] <- cells
	colrow[,4] <- NA
	rows <- na.omit(unique(colrow[order(colrow[,2]), 2]))
	for (i in 1:length(rows)) {
		v <- .readRowAscii(raster, rows[i])
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i],4] <- v[thisrow[,1]]
	}
	return(colrow[,4]) 
}


