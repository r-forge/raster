# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, is.na(.getRasterValues(x))))
	}
)	

setMethod("is.nan", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, is.nan(.getRasterValues(x))))
	}
)	

setMethod("is.infinite", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, values=is.infinite(.getRasterValues(x))))
	}
)	

setMethod("is.finite", signature(x='RasterLayer'),
	function(x) {
		rst <- raster(x)
		dataType(rst) <- 'LOG1S'
		return(setValues(rst, values=is.finite(.getRasterValues(x))))
	}
)	

