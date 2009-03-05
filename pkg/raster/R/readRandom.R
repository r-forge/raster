# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3



#sample while reading and return matrix (for plotting )

sampleRandom <- function(raster, n=500, na.rm = TRUE) {
	if (dataContent(raster) == 'all') {
		values <- values(raster)
		if (na.rm) { values <- na.omit(values) }
		if (length(values) > n) {
			r <- order(runif(length(values)))
			values <- values[r]
			values <- values[1:n]
		}
	} else {
		if (dataSource(raster) == 'disk') {
			if (ncell(raster) <= n) {
				raster <- readAll(raster)
				values <- cbind(1:ncell(raster), values(raster))
				if (na.rm) { values <- na.omit(values) }
			} else {	
				if (na.rm) {
					N <- n 
				} else {
					N <- 2 * n 
				}	
				cells <- unique(as.integer(round(runif(N) * ncell(raster) + 0.5)))
				cells <- cells[cells > 0]
				values <- cellValues(raster, cells)
				if (na.rm) {
					values <- na.omit(values)
					if (length(values) >= n) {
						values <- values[1:n]
					}
				}	
			}
		}
	}	
	return(values)
}



sampleSkip <- function(raster, maxdim=500, bndbox=NA, asRaster=FALSE) {
	if (!(is.na(bndbox))) { 
		rcut <- crop(raster, bndbox) 
		warning('bndbox option has not been implemented yet')
	} else {
		rcut <- setRaster(raster)
	}
	# Need to do something with this now.....
	
	rasdim <- max(ncol(raster), nrow(raster) )
	if (rasdim <= maxdim) { 
		if (dataContent(raster) == 'all') {
			outras <- raster
		} else { 
			outras <- readAll(raster) 
		}
	} else {
		fact <- maxdim / rasdim
		nc <- max(1, trunc(fact * ncol(raster)))
		nr <- max(1, trunc(fact * nrow(raster)))
		colint <- round(ncol(raster) / nc)
		rowint <- round(nrow(raster) / nr)
		nc <- trunc(ncol(raster) / colint)
		nr <- trunc(nrow(raster) / rowint)
		cols <- 1:nc
		cols <- 1 + (cols-1) * colint 
		dd <- vector()
		if (dataContent(raster) == 'all') {
			for (i in 1:nr) {
				row <- 1 + (i-1) * rowint
				v <- values(raster, row)
				dd <- c(dd, v[cols])
			}	
		} else {
			for (i in 1:nr) {
				row <- 1 + (i-1) * rowint
				raster <- readRow(raster, row)
				dd <- c(dd, values(raster)[cols])
			}	
		}	
	}
	if (asRaster) {
		outras <- setRaster(raster)
		outras <- setRowCol(outras, nr, nc)
		xmx <- xmax(raster) - (ncol(raster) - cols[nc]) * xres(raster)
		ymn <- ymin(raster) + (nrow(raster) - row) * yres(raster)
		bndbox <- changeBbox(raster, xmx=xmx, ymn=ymn)
		outras <- setBbox(outras, bndbox, keepres=F)
		outras <- setValues(outras, dd)
		return(outras)
	} else {
		return(dd)
	}	
}


