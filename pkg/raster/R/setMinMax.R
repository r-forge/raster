	
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

	
setMinMax <- function(raster) {
	clear <- FALSE
	if (dataContent(raster) != 'all' & dataContent(raster) != 'sparse') {
		if (dataSource(raster) == 'ram') {
			stop('no values associated with this RasterLayer')
		}
		if (.CanProcessInMemory(raster, 2)) {
			raster <- readAll(raster)
			clear <- TRUE
		}
	}
	
	if (dataContent(raster)=='all' | dataContent(raster)=='sparse') {
		vals <- na.omit(values(raster)) # min and max values
		if (clear) {raster <- clearValues(raster)}
		if (length(vals) > 0) {
			raster@data@min <- min(vals)
			raster@data@max <- max(vals)
		} else {
			raster@data@min <- NA
			raster@data@max <- NA
		}
	} else {
		raster@data@min <- Inf
		raster@data@max <- -Inf
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			rsd <- na.omit(values(raster)) # min and max values
			if (length(rsd) > 0) {
				raster@data@min <- min(minValue(raster), min(rsd))
				raster@data@max <- max(maxValue(raster), max(rsd))
			}	
		}
		raster <- clearValues(raster)
	}
#	if (raster@file@datatype == 'logical') {
#		raster@data@min <- as.logical(raster@data@min)
#		raster@data@max <- as.logical(raster@data@max)
#	}
	raster@data@haveminmax <- TRUE
	return(raster)
}

