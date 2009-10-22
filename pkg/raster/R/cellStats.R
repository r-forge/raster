# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

cellStats <- function(raster, stat='mean', ...) {

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

		st  <- NULL
		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'countNA') {
			st <- 0		
			nc <- ncol(raster)
		} else if (stat == 'mean' | stat == 'sd') {
			# do nothing
		} else { 
			stop("invalid 'stat'. Should be 'sum', 'min', 'max', 'sd', 'mean' or 'countNA'") 
		}

		cnt <- 0
		sumsq <- 0
		
		pb <- pbCreate(nrow(raster), type=.progress(...))
		for (r in 1:nrow(raster)) {
			d <- na.omit(getValues(raster, r))
			if (length(d) == 0) { next }
			if (stat == 'sd') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
				sumsq <- sum(d^2, sumsq)
			} else if (stat=='mean') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
			} else if (stat=='countNA') {
				st <- st + (nc - length(d))
			} else {
				st <- fun(c(d, st))
			}
			pbStep(pb, r) 
		}
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt( (1 / cnt) * sumsq - meansq )
		} else if (stat == 'mean') {
			st <- st / cnt
		}
		pbClose(pb)
		return(st)
	}
}

