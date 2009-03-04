# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.summaryRasters <- function(rasters, fun, funname, na.rm) {

	if (!.CanProcessInMemory(rasters[[1]], 2)) {
		filename <- tempfile()
		raster <- setRaster(rasters[[1]], filename)
	} else {
		filename <- ""
		raster <- setRaster(rasters[[1]])
		v <- vector(length=0)
	}

	m <- matrix(NA, nrow=ncol(rasters[[1]]), ncol=length(rasters))
	for (r in 1:nrow(rasters[[1]])) {
		m[] <- NA
		for (i in 1:length(rasters)) {
			if (is.atomic(rasters[[i]])) {
				m[,i] <- rasters[[i]]
			} else {
				m[,i] <- .getRowValues(rasters[[i]], r)
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
			raster <- writeRaster(raster)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}



setMethod("Summary", signature(x='RasterLayer'),
	function(x, ..., na.rm=FALSE){

		rasters <- list(...)
		if (length(rasters)==0) { return(x) }

		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) == 'RasterStack') {
				r <- rasters[[i]]
				rasters <- rasters[-i]
				rasters <- c(rasters, unstack(r))
				rm(r)
			}
		}
		rasters <- c(x, rasters)
		rm(x)

		fun <- sys.call(sys.parent())[[1]]
		funname <- as.character(sys.call(sys.parent())[[1]])
		
		return( .summaryRasters(rasters, fun, funname, na.rm) )
	}
)
	


setMethod("Summary", signature(x='RasterStack'),
	function(x, ..., na.rm=FALSE){
		
		x1 <- asRasterLayer(x, 1)
		x <- dropLayer(x, 1)
		
		return( callGeneric(x1, x, ..., na.rm=na.rm))
	}
)



