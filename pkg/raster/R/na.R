# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, is.na(getValues(x))))
	}
)	

setMethod("is.nan", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, is.nan(getValues(x))))
	}
)	

setMethod("is.infinite", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, values=is.infinite(getValues(x))))
	}
)	

setMethod("is.finite", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, values=is.finite(getValues(x))))
	}
)	

