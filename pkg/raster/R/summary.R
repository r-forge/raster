# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.summaryRasters <- function(rasters, fun, funname, na.rm, ...) {

	if (!canProcessInMemory(rasters[[1]], 4)) {
		filename <- rasterTmpFile()
	} else {
		filename <- ""
		v <- vector(length=0)
	}
	raster <- raster(rasters[[1]])

	m <- matrix(NA, nrow=ncol(rasters[[1]]), ncol=length(rasters))
	
	for (i in 2:length(rasters)) {
		if (extends(class(rasters[[i]]), "Raster")) {
			compare(c(rasters[[1]], rasters[[i]]))
		}
	}

	
	for (r in 1:nrow(rasters[[1]])) {
		m[] <- NA
		for (i in 1:length(rasters)) {
			if (is.atomic(rasters[[i]])) {
				m[,i] <- rasters[[i]]
			} else {
				m[,i] <- getValues(rasters[[i]], r)
			}
		}
		if (funname == 'any' || funname == 'all') {
			m[m != 0] <- 1
		}

		vv <- apply(m, 1, fun, na.rm=na.rm)

		if (funname == 'range') {
			vv <- vv[2,] - vv[1,]
		}

		if (filename == "") {
			v <- c(v, vv)
		} else {
			raster <- setValues(raster, vv, r)
			raster <- writeRaster(raster, filename=filename, ...)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}



setMethod("Summary", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		rasters <- .makeRasterList(x, ...)
		if (length(rasters) == 1) { return(x) }
		rm(x)
		fun <- sys.call(sys.parent())[[1]]
		funname <- as.character(sys.call(sys.parent())[[1]])
		return( .summaryRasters(rasters, fun, funname, na.rm) )
	}
)


