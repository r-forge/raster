# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.9
# Licence GPL v3


sampleRegular <- function(raster, n, extent=NULL, asRaster=FALSE, corners=FALSE) {
	if (n<1) {stop('n < 1')}
	if (n >= ncell(raster)) {
		if (asRaster) {
			if (is.null(extent)) {
				if (dataContent(raster) != 'all') { raster <- readAll(raster) }
				return(raster)
			} else {
				return(crop(raster, extent))
			}
		} else {
			if (is.null(extent)) {
				if (dataContent(raster) != 'all') { raster <- readAll(raster) }
			} else {
				raster <- crop(raster, extent)
				if (dataContent(raster) != 'all') { raster <- readAll(raster) }
			}			
			return(values(raster))
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

	x <- floor(sqrt(ncell(raster)/n))
	y <- x
	nr <- max(1,floor((lastrow - firstrow + 1) / y))
	rows <- firstrow + y * (0:(nr-1)) 
	if (corners) {
		if (rows[length(rows)] != nrow(raster)) {
			rows <- c(rows, nrow(raster))	
		}
	} else {
		rows <- rows + floor(y/2) 
	}
	nc <- max(1, floor((lastcol - firstcol + 1) / x))
	cols <- firstcol + x * (0:(nc-1))
	if (corners) {
		if (cols[length(cols)] != ncol(raster)) {
			cols <- c(cols, ncol(raster))	
		}
	} else {
		cols <- cols + floor(x/2) 
	}
	nr <- length(rows)
	nc <- length(cols)
	m <- matrix(ncol=nr, nrow=nc)
	for (i in 1:nr) {
		v <- getValues(raster, rows[i])
		m[,i] <- v[cols]
	}	
	m <- as.vector(m)
	if (asRaster) {
		outras <- raster(nrow=nr, ncol=nc, xmn=xmin(raster), xmx=xFromCol(raster, cols[length(cols)])+0.5*xres(raster) , ymn=yFromRow(raster, rows[length(rows)])-0.5*yres(raster), ymx=ymax(raster)) 
		outras <- setValues(outras, m)
		return(outras)
	} else {
		return(m)
	}	
}
