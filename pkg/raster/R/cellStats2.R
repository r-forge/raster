# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

cellStats <- function(raster, stat='mean', track=-1) {

	if (class(stat) != 'character') {
		if (canProcessInMemory(raster, 2)) {
			if (dataContent(raster) != 'all') {
				raster <- readAll(raster)
			}
			d <- na.omit(values(raster))
			return( stat(d) )
		} else {
			stop("RasterLayer is too large. You can use fun='sum', 'mean', 'min', or 'max', but not a function")
		}
	} else {

		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'mean') {
			fun <- sum
			counts <- TRUE
		} else { 
			stop("invalid 'stat', should be 'sum', 'min', 'max', or 'mean'") 
		}

		cnt <- vector(length=0)
		st  <- vector(length=0)
		starttime <- proc.time()
		for (r in 1:nrow(raster)) {
			d <- na.omit(valuesRow(raster, r))
			st <- fun(d, st)
			if (counts) {
				cnt <- cnt + length(d)
			}
			if (r %in% track) { .showTrack(r, raster@nrows, track, starttime) }
		}
		if (counts) {
			st <- st / cnt
		}
		return(st)
	}
}

