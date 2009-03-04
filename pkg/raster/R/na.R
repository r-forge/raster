# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, 'LOGICAL')
		return(setValues(raster, is.na(.getRasterValues(x))))
	}
)	

setMethod("is.nan", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, 'LOGICAL')
		return(setValues(raster, is.nan(.getRasterValues(x))))
	}
)	

setMethod("is.infinite", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, 'LOGICAL')
		return(setValues(raster, values=is.infinite(.getRasterValues(x))))
	}
)	

setMethod("is.finite", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, 'LOGICAL')
		return(setValues(raster, values=is.finite(.getRasterValues(x))))
	}
)	

