# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

zonal <- function(raster, zones, stat='mean', keepdata=TRUE, progress) {
	compare(c(raster, zones))
	
	if (missing(progress)) {progress <- .progress()}

	if (class(stat) != 'character') {
		if (canProcessInMemory(raster, 3)) {
			if (dataContent(raster) != 'all') {
				raster <- readAll(raster)
			}
			d <- values(raster)
			rm(raster)
			if (dataContent(zones) != 'all') {
				zones <- readAll(zones)
			}
			d <- cbind(d, as.integer(values(zones)))
			rm(zones)
			if (keepdata) {
				d <- na.omit(d)
			}
			alltab  <-  tapply(d[,1], d[,2], stat) 
			stat <- deparse(substitute(stat))
		} else {
			stop("RasterLayers are too large. You can use fun='sum', 'mean', 'min', or 'max', but not a function")
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

		alltab <- array(dim=0)
		cnttab <- alltab
	
		
		pb <- pbCreate(nrow(raster), type=progress)
		
		for (r in 1:nrow(raster)) {
			d <- cbind(getValues(raster, r), as.integer(getValues(zones, r)))
			if (keepdata) {
				d <- na.omit(d)
			}
			if (length(d) == 0) { next }
			alltab <- c(alltab, tapply(d[,1], d[,2], fun))
			if (counts) {
				cnttab <- c(cnttab, tapply(d[,1], d[,2], length))
			}
			if (length(alltab) > 10000) {
				groups <- as.integer(names(alltab))
				alltab <- tapply(as.vector(alltab), groups, fun)
				if (counts) {
					cnttab <- tapply(as.vector(cnttab), groups, sum)
				}
			}
			pbStep(pb, r)
		}
		pbClose(pb)
			
		groups <- as.integer(names(alltab))
		alltab <- tapply(as.vector(alltab), groups, fun)
		if (counts) {
			cnttab <- tapply(as.vector(cnttab), groups, sum)
			alltab <- alltab / cnttab
		}
	}
	zone <- as.integer(names(alltab))
	alltab <- data.frame(zone, alltab)
	colnames(alltab) <- c('zone', stat)
	return(alltab)
}

