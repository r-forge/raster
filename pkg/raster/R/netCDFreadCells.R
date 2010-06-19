# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRasterCellsNetCDF <- function(x, cells) {

# read all
	if (canProcessInMemory(x, 2)) {
		r <- getValues(x)
		r <- r[cells]
		return(r)
	} 
	
# read only rows needed	
	row1 <- rowFromCell(x, min(cells))
	row2 <- rowFromCell(x, max(cells))
	ncl <- (row2 - row1 + 1) * x@ncols
	r <- raster(nrow=1, ncol=ncl)

	if (canProcessInMemory(r, 2)) {
		v <- getValues(x, row1, row2-row1+1)
		v <- v[cells-cellFromRowCol(x, row1, 1)+1]
		return(v)
	}
	
# read row by row
	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))
	readrows = rows
	if ( x@file@toptobottom ) { readrows <- x@nrows - readrows + 1	}

	zvar = x@data@zvar
	time = x@data@band
	
	nc <- open.nc(x@file@name)
	count = c(x@ncols, 1, 1)
	for (i in 1:length(rows)) {
		start = c(1, readrows[i], time)
		values <- as.vector(var.get.nc(nc, variable=zvar, start=start, count=count))
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i], 3] <- values[thisrow[,1]]
	}	
	close.nc(nc)	
	
	return(colrow[, 3]) 
}



.readBrickCellsNetCDF <- function(x, cells, layers='') {

	if (layers=='') { layers = 1:nlayers(x)
	} else {
		layers = round(layers)
		layers = subset(layers, layers >= 1 & layers <= nlayers(x))
	}
	
# read all
	if (canProcessInMemory(x, 2)) {
		r <- getValues(x)
		r <- r[cells, ]
		return(r)
	} 
	
# read only rows needed	
	row1 <- rowFromCell(x, min(cells))
	row2 <- rowFromCell(x, max(cells))
	ncl <- (row2 - row1 + 1) * x@ncols
	r <- raster(nrow=1, ncol=ncl)

	if (canProcessInMemory(r, 2)) {
		v <- getValues(x, row1, row2-row1+1)
		v <- v[cells-cellFromRowCol(x, row1, 1)+1]
		return(v)
	}
	
# read row by row
	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))
	readrows = rows
	if ( x@file@toptobottom ) { readrows <- x@nrows - readrows + 1	}

	zvar = x@data@zvar
	time = x@data@band
	
	nc <- open.nc(x@file@name)
	count = c(x@ncols, 1, 1)
	for (i in 1:length(rows)) {
		start = c(1, readrows[i], time)
		values <- as.vector(var.get.nc(nc, variable=zvar, start=start, count=count))
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i], 3] <- values[thisrow[,1]]
	}	
	close.nc(nc)	
	
	return(colrow[, 3]) 
}

