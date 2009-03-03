# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 

		fname <- as.character(sys.call(sys.parent())[[1]])
		 
		if (.CanProcessInMemory(x, 3)) {
			raster <- setRaster(x, values=callGeneric(.getRasterValues(x)))
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				raster <- setDatatype(raster, 'INT4S')
			}
		} else {
			raster <- setRaster(x, filename=tempfile())
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				raster <- setDatatype(raster, 'INT4S')
			}
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, callGeneric( .getRowValues(x, r) ), r)
				raster <- writeRaster(raster)
			}
			if (options('verbose')[[1]]) {
				cat('values were written to:', filename(raster))
			}
		}
		return(raster)
	}
)


setMethod("Math2", signature(x='RasterLayer'), 
	function (x, digits=0) {
		digits <- max(0, digits)
		if (.CanProcessInMemory(x, 1)) {
			x <- setValues(x, callGeneric(values(x), digits))
			if (digits == 0) {
				x <- setDatatype(x, 'INT4S')
			}
			return(x)
		} else {
			raster <- setRaster(x, filename=tempfile())
			if (digits == 0) {
				x <- setDatatype(x, 'INT4S')
			}
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, callGeneric(.getRowValues(x, r), digits), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)
