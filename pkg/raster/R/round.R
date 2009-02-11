# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setMethod(round, signature(x='RasterLayer'), 
	function (x, digits = 0) {
		digits <- max(0, digits)
		if (.CanProcessInMemory(x, 1)) {
			x <- setValues(x, round(values(x), digits))
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
				raster <- setValues(raster, round(.getRowValues(x, r), digits), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)


setMethod(trunc, signature(x='RasterLayer'), 
	function (x) {
		if (.CanProcessInMemory(x, 1)) {
			x <- setValues(x, trunc(values(x)))
			x <- setDatatype(x, 'INT4S')
			return(x)
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, 'INT4S')
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, trunc(.getRowValues(x, r)), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)



setMethod(ceiling, signature(x='RasterLayer'), 
	function (x) {
		if (.CanProcessInMemory(x)) {
			x <- setValues(x, ceiling(values(x)))
			x <- setDatatype(x, 'INT4S')
			return(x)
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, 'INT4S')
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, ceiling(.getRowValues(x, r)), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)


setMethod(floor, signature(x='RasterLayer'), 
	function (x) {
		if (.CanProcessInMemory(x)) {
			x <- setValues(x, floor(values(x)))
			x <- setDatatype(x, 'INT4S')
			return(x)
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, 'INT4S')
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, floor(.getRowValues(x, r)), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)

