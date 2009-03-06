# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setReplaceMethod("[", c("RasterLayer", "ANY", "missing"),
	function(x, i, j, value) {
		if  (missing(i)) {	
			if (length(value) == ncell(x)) {
				return(setValues(x, value))
			} else if (length(value) == 1) {
				return( setValues(x, rep(value, times=ncell(x))) )
			} else {
				stop('length of replacement values should be 1 or ncell')
			}
		}
		
		if (class(i) == "RasterLayer") {
			i <- as.logical( .getRasterValues(i) ) 
		}
# what about data rows ?		
		if (dataContent(x) == 'nodata') {
			if (.CanProcessInMemory(x, 2)) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, rep(NA, times=ncell(x)))
				}
			} else {
				stop('raster too large.')
			}	
		}
		x@data@values[i] <- value
		x@data@source <- 'ram'
		x <- setFilename(x, "")
		x <- setMinMax(x)
		return(x)
	}
)


