# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function( x, size, extent=NULL, cells=FALSE, asRaster=FALSE) {
	
	size <- round(size)
	stopifnot(size > 0)
	
	rotated <- rotated(x)
	driver <- try(raster:::.driver(x), silent=TRUE)
	
	if (is.null(extent)) {
		if (size >= ncell(x)) {
			if (asRaster) { 
				if (!rotated) {
					return(x) 
				}
			} else { 
				return(getValues(x)) 
			}
		}
		rcut <- raster(x)
		firstrow <- 1
		lastrow <- nrow(rcut)
		firstcol <- 1
		lastcol <- ncol(rcut)
		
	} else {
	
		extent <- alignExtent(extent, x)
		rcut <- crop(raster(x), extent)
		if (size >= ncell(rcut)) {
			x <- crop(x, extent)
			if (asRaster) { 
				return(x) 
			} else { 
				return(getValues(x)) 
			}
		}
		firstrow <- rowFromY(x, ymax(rcut))
		lastrow <- rowFromY(x, ymin(rcut)+0.5 *yres(rcut))
		firstcol <- colFromX(x, xmin(rcut))
		lastcol <- colFromX(x, xmax(rcut)-0.5 *xres(rcut))
	}
	

	Y <- X <- sqrt(ncell(rcut)/size)
	nr <- max(1, floor((lastrow - firstrow + 1) / Y))
	nc <- max(1, floor((lastcol - firstcol + 1) / X))

	rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
	rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
	cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
	cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)

	cols <- unique(round(cols))
	rows <- unique(round(rows))
	cols = cols[cols>0]
	rows = rows[rows>0]
	nr <- length(rows)
	nc <- length(cols)
	

#	driver <- try(raster:::.driver(x), silent=TRUE)
#	if (driver=='gdal' & !rotated) {
#		v <- readGDAL(filename(x), offset=c(firstrow-1,firstcol-1), region.dim=c(nrow(rcut), ncol(rcut)), output.dim=c(nr, nc), silent=TRUE) 
#		if (asRaster) {
#			if (nlayers(x) > 1) {
#				return(brick(v))
#			} else {
#				return(raster(v))
#			}
#		} else {
#			return(v@data)
#		}
#	}

	cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
	
	if ( ! inMemory(x) ) { 
		if (canProcessInMemory(x, 4)) {
			x <- readAll(x)
		}
	}
	
	if (asRaster) {
		if (rotated) {
			if (is.null(extent)) {
				outras <- raster(raster::extent(x))
			} else {
				outras <- raster(extent)
			}
			ncol(outras) <- nc
			nrow(outras) <- nr
			xy <- xyFromCell(outras, 1:ncell(outras))
			m <- .xyValues(x, xy)
			
		} else {
			m <- .cellValues(x, cell)

			if (is.null(extent))  {
				outras <- raster(x)
			} else {
				outras <- raster(extent) 
			}
			nrow(outras) <- nr
			ncol(outras) <- nc
			
		}
		if (nlayers(x) > 1) {
			outras <- brick(outras, nl=nlayers(x))
		}
		outras <- setValues(outras, m)
		layerNames(outras) <- layerNames(x)
		return(outras)
		
	} else {
	
		m <- .cellValues(x, cell)
		if (cells) {
			m <- cbind(cell, m)
			colnames(m)[2:ncol(m)] <- layerNames(x)
		} 
		return(m)
	}	
}

