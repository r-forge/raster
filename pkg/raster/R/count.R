# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.9
# Licence GPL v3


count <- function(raster, value, digits=0) {
	if (canProcessInMemory(raster, 2)) {
		if (dataContent(raster) != 'all') {
			raster <- readAll(raster)
		}
		if (is.na(value)) {
			x <- sum(is.na(values(raster)))
		} else {
			v <- na.omit(round(values(raster), digits=digits))
			x <- sum(v == value)
		}
	} else {
		x <- 0
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (is.na(value)) {
				x <- x + sum(is.na(values(raster)))
			} else {
				v <- na.omit(round(values(raster), digits=digits))
				x <- x + sum(v == value)
			}
		}
	}
	return(x)
}

