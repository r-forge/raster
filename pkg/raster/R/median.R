# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

setGeneric("Median", function(x, ...)
	standardGeneric("Median"))


setMethod('Median', signature(x='ANY'), 
	function(x, ..., na.rm=FALSE){
		x <- c(x, ...)
		return(stats::median(x, na.rm=na.rm))
	}
)


setMethod("Median", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){

		rasters <- .makeRasterList(x, ...)
		add <- .addArgs(fun, ...)

		if (length(rasters) <= 1 & length(add)==0) { return(x) }
		rm(x)
		
		fun <- function(...){ stats::median(..., na.rm=na.rm) }
		return( .summaryRasters(rasters=rasters, add=add, fun=fun, ...) )
		
	}
)


