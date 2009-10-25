# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.9
# Licence GPL v3




setMethod("mean", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){

		rasters <- list(...)

		if (class(x) == 'RasterLayer') {
			if (length(rasters)==0) { 
				return(x) 
			}
		}
		rasters <- c(x, rasters)
		rm(x)

		newrasters <- list()
		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) == 'RasterLayer') {
				newrasters <- c(newrasters, rasters[[i]])
			}
			if (class(rasters[[i]]) == 'RasterStack') {
				newrasters <- c(newrasters, unstack(rasters[[i]]))
			} else {
				stop('not yet implemented for a Brick')
			}
		}
		rm(rasters)
		
		return( .summaryRasters(newrasters, mean, 'mean', na.rm=na.rm) )
	}
)

