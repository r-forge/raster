# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.9
# Licence GPL v3

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


