# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

cellStats <- function(raster, stat='mean', track=-1) {

	if (class(stat) != 'character') {
		if (dataContent(raster) == 'all') { n <- 1 } else {n <- 2}
		if (canProcessInMemory(raster, n)) {
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
		} else if (stat == 'mean' | stat == 'sd') {
			# do nothing
		} else { 
			stop("invalid 'stat', should be 'sum', 'min', 'max', 'sd' or 'mean'") 
		}

		cnt <- 0
		sumsq <- 0
		st  <- NULL
		starttime <- proc.time()
		for (r in 1:nrow(raster)) {
			d <- na.omit(valuesRow(raster, r))
			if (length(d) == 0) { next }
			if (stat == 'sd') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
				sumsq <- sum(d^2, sumsq)
			} else if (stat=='mean') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
			} else {
				st <- fun(c(d, st))
			}
			if (r %in% track) { 
				.showTrack(r, raster@nrows, track, starttime) 
			}
		}
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt( (1 / cnt) * sumsq - meansq )
		} else if (stat == 'mean') {
			st <- st / cnt
		}
		return(st)
	}
}

