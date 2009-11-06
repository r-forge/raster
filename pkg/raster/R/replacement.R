# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.9
# Licence GPL v3



setReplaceMethod("[", c("RasterLayer", "ANY", "missing"),
	function(x, i, j, value) {
		
		if (dataContent(x) != 'all') {
			if (canProcessInMemory(x, 3)) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, rep(NA, times=ncell(x)))
				}
			}
		}
				
		if (dataContent(x) == 'all') {
			if  (missing(i)) {
				if (length(value) == ncell(x)) {
					return(setValues(x, value))
				} else if (length(value) == 1) {
					return( setValues(x, rep(value, times=ncell(x))) )
				} else {
					v <- vector(length=ncell(x))
					v[] <- value
					return( setValues(x, v) )
				}
			}
		
			if (class(i) == "RasterLayer") {
				i <- as.logical( getValues(i) ) 
			}
			if (!is.logical(i)) {
				i <- subset(i, i <= ncell(x))
				i <- subset(i, i >= 1)
			}
			x@data@values[i] <- value
			x@data@source <- 'ram'
			filename(x) <- ""
			x <- setMinMax(x)
		} else {
			filename <- rasterTmpFile()
			outras <- raster(x)
			if  (missing(i)) { 
				i <- vector(length=ncol(outras))
				i[] <- TRUE
			}
			for (r in 1:nrow(outras)) {
				if (class(i) == "RasterLayer") {
					ind <- as.logical( getValues(i, r) ) 
				} else if (is.logical(i)) {
					if (length(i) == ncol(outras)) {
						ind <- i
					} else if (length(i) == ncell(outras)) {
						ind <- i[(cellFromRow(r,1)[1]):(cellFromRow(r,1)[ncol(r)])]
					} else {
						stop('cannot recycle logical indices for large rasters')
					}
				} else {
					ind <- subset(i, i <= cellFromRow(r,1)[1])
					ind <- subset(ind, ind >= cellFromRow(r,1)[ncol(r)])
				}
				v <- getValues(x, r)
				if (length(ind) > 0) {
					v[ind] <- values[ind]
					outras <- setvalues(outras, v)
					outras <- writeRaster(outras, filename)
				}
			}
		}
		return(x)
	}
)
