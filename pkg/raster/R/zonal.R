# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

zonal <- function(raster, zones, stat='mean', keepdata=TRUE, track=-1) {

	if (class(stat) != 'character') {
		if (canProcessInMemory(raster, 4)) {
			d <- cbind(values(readAll(raster)), as.integer(values(readAll(zones))))
			if (keepdata) {
				d <- na.omit(d)
			}
			alltab  <-  tapply(d[,1], d[,2], stat) 
			stat <- deparse(substitute(stat))
		} else {
			stop("RasterLayers are too large. You an use fun='sum', 'mean', 'min', or 'max', but not a function")
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
		starttime <- proc.time()
		for (r in 1:nrow(raster)) {
			d <- cbind(valuesRow(raster, r), as.integer(valuesRow(zones, r)))
			if (keepdata) {
				d <- na.omit(d)
			}
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
			if (r %in% track) { .showTrack(r, raster@nrows, track, starttime) }
		}
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

