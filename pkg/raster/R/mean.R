# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.9
# Licence GPL v3

setMethod("mean", signature(x='Raster'),
	function(x, ..., trim = 0, na.rm=FALSE){
		rasters <- .makeRasterList(x, ...)
		add <- .addArgs(...)
		if (length(rasters) == 1 & length(add)==0) { return(x) }
		rm(x)
		fun <- function(...){ mean(..., trim=trim, na.rm=na.rm) }
		return( .summaryRasters(rasters=rasters, add=add, fun=fun, ...) )		
	}
)

