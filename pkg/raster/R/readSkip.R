# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3



sampleSkip <- function(raster, maxdim=500, bndbox=NA, asRaster=FALSE) {
	if (!(is.na(bndbox))) { 
		rcut <- crop(raster, bndbox) 
		warning('bndbox option has not been implemented yet')
	} else {
		rcut <- raster(raster)
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
		outras <- raster(raster)
		outras <- setRowCol(outras, nr, nc)
		xmx <- xmax(raster) - (ncol(raster) - cols[nc]) * xres(raster)
		ymn <- ymin(raster) + (nrow(raster) - row) * yres(raster)
		bndbox <- changeExtent(raster, xmx=xmx, ymn=ymn)
		outras <- setExtent(outras, bndbox, keepres=FALSE)
		outras <- setValues(outras, dd)
	}
	if (asRaster) {
		return(outras)
	} else {
		return(values(outras))
	}	
}
