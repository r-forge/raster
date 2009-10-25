# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.9
# Licence GPL v3

freq <- function(raster, digits=0) {
	if (canProcessInMemory(raster, 2)) {
		if (dataContent(raster) != 'all') {
			raster <- readAll(raster)
		}
		d <- round(values(raster), digits=digits)
		x <- table(d, useNA="ifany" )
	} else {
		xx <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			d <- round(values(raster), digits=digits)
			x <- table(d, useNA="ifany" )
			x <- cbind(as.numeric(unlist(as.vector(dimnames(x)))), as.vector(x))
			xx <- rbind(xx, x)
		}
		x <- tapply(xx[,2], xx[,1], sum)
	}
	x <- cbind(as.numeric(unlist(as.vector(dimnames(x)))), as.vector(x))
	colnames(x) <- c('value', 'count')
	return(x)
}

