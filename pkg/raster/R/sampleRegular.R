# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function(raster, n, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE) {
	if (n<1) {stop('n < 1')}
	
	if (is.null(extent)) {
		if (n >= ncell(raster)) {
			if (dataContent(raster) != 'all') { 
			raster <- readAll(raster) }
			if (asRaster) { return(raster) 
			} else { return(values(raster)) }
		}
	} else {
		extent <- alignExtent(extent, raster)
		rr <- crop(raster(raster), extent)
		if (n >= ncell(rr)) {
			raster <- crop(raster, extent)
			if (asRaster) { return(raster) 
			} else { return(values(raster)) }
		}
	}
	
	rcut <- raster(raster)
	if (!(is.null(extent))) { 
		rcut <- crop(rcut, extent) 
		firstrow <- rowFromY(raster, ymax(rcut))
		lastrow <- rowFromY(raster, ymin(rcut))
		firstcol <- colFromX(raster, xmin(rcut))
		lastcol <- colFromX(raster, xmax(rcut))
	} else {
		firstrow <- 1
		lastrow <- nrow(rcut)
		firstcol <- 1
		lastcol <- ncol(rcut)
	}

	x <- sqrt(ncell(rcut)/n)
	y <- x
	nr <- max(1,floor((lastrow - firstrow + 1) / y))
	rows <- (lastrow - firstrow + 1)/nr * 1:nr
	if (corners) {
		rows <- c(1, rows)	
		rows[length(rows)] <- nrow(rcut) 
	} else {
		rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
	}
	rows <- round(rows)
	
	nc <- max(1, floor((lastcol - firstcol + 1) / x))
	cols <- (lastcol - firstcol + 1)/nc * 1:nc
	if (corners) {
		cols <- c(1, cols)
		cols[length(cols)] <- ncol(rcut) 
	} else {
		cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)
	}
	cols <- round(cols)
	cols <- unique(cols)
	rows <- unique(rows)
	nr <- length(rows)
	nc <- length(cols)
	
#	m <- matrix(ncol=nr, nrow=nc)
#	for (i in 1:nr) {
#		v <- getValues(raster, rows[i])
#		m[,i] <- v[cols]
#	}	
#	m <- as.vector(m)
	cell <- cellFromRowCol(raster, rep(rows, each=nc), rep(cols, times=nr))
	m <- .readCells(raster, cell)

	if (asRaster) {
		if (is.null(extent))  {
			outras <- raster(nrow=nr, ncol=nc, xmn=xmin(raster), xmx=xFromCol(raster, cols[length(cols)])+0.5*xres(raster) , ymn=yFromRow(raster, rows[length(rows)])-0.5*yres(raster), ymx=ymax(raster)) 
		} else {
			outras <- raster(extent) 
			nrow(outras) <- nr
			ncol(outras) <- nc
		}
		outras <- setValues(outras, m)
		return(outras)
	} else {
		if (cells) {
			#cell <- cellFromRowCol(raster, rep(rows, each=nc), rep(cols, times=nr))
			cell <- cbind(cell, m)
			colnames(cell)[2] <- 'value'
			return(cell)
		} else {
			return(m)
		}
	}	
}
