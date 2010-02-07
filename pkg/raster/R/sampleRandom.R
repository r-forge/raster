# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

sampleRandom <- function(raster, n=500, na.rm=TRUE) {
	if (dataContent(raster) == 'all') {
		values <- values(raster)
		if (na.rm) { values <- na.omit(values) }
		if (length(values) > n) {
			s = sample.int(length(values), n)
			values <- values[s]
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
				cells <- sample.int(ncell(raster), N)
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


