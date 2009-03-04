# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3




setMethod("mean", signature(x='Raster'),
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

		return( .summaryRasters(rasters, mean, 'mean', na.rm=na.rm) )
	}
)


setMethod("mean", signature(x='RasterStack'),
	function(x, ..., na.rm=FALSE){

		x1 <- asRasterLayer(x, 1)
		x <- dropLayer(x, 1)
		
		return(  mean(x1, x, ..., na.rm=na.rm) )
	}
)

