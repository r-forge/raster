# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



setMethod(round, signature(x='RasterLayer'), 
	function (x, digits = 0) {
		if (.CanProcessInMemory(x, 1)) {
			return(setValues(x, round(values(x), digits)))
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
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
			return(setValues(x, trunc(values(x))))
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
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
			return(setValues(x, ceiling(values(x))))
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
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
			return(setValues(x, floor(values(x))))
		} else {
			raster <- setRaster(x, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, floor(.getRowValues(x, r)), r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}
	}
)

