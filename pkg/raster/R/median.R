# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

setGeneric("median", function(x, na.rm=FALSE)
	standardGeneric("median"))


#setMethod('median', signature(x='ANY'), 
#	function(x, ..., na.rm=FALSE){
#		x <- c(x, ...)
#		return(stats::median(x, na.rm=na.rm))
#	}
#)


setMethod("median", signature(x='Raster'),
#	function(x, ..., na.rm=FALSE){
#		rasters <- list(...)
	function(x, na.rm=FALSE){
		if (class(x) == 'RasterLayer') {
			if (length(rasters)==0) { 
				return(x) 
			}
		}
		rasters <- c(x, rasters)
		rm(x)
		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) == 'RasterStack') {
				r <- rasters[[i]]
				rasters <- rasters[-i]
				rasters <- c(rasters, unstack(r))
				rm(r)
			}
		}
		return( .summaryRasters(rasters, stats::median, 'median', na.rm=na.rm) )
	}
)

