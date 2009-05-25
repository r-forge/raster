# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		fname <- as.character(sys.call(sys.parent())[[1]])
		rst <- raster(x)
		if (canProcessInMemory(rst, 3)) {
			rst <- setValues(rst, callGeneric(.getRasterValues(x)))
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				dataType(rst) <- 'INT4S'
			}
		} else {
			filename(rst) <- rasterTmpFile()
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				dataType(rst) <- 'INT4S'
			}
			for (r in 1:nrow(x)) {
				rst <- setValues(rst, callGeneric( .getRowValues(x, r) ), r)
				rst <- writeRaster(rst)
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename(raster))
			}
		}
		return(rst)
	}
)


setMethod("Math2", signature(x='RasterLayer'), 
	function (x, digits=0) {
		digits <- max(0, digits)
		rst <- raster(x)
		if (canProcessInMemory(x, 3)) {
			rst <- setValues(rst, callGeneric( .getRasterValues(x), digits))
			if (digits == 0) {
				dataType(rst) <- 'INT4S'
			}
			return(rst)
		} else {
			filename(rst) <- rasterTmpFile()
			if (digits == 0) {
				dataType(rst) <- 'INT4S'
			}
			for (r in 1:nrow(x)) {
				rst <- setValues(rst, callGeneric(.getRowValues(x, r), digits), r)
				rst <- writeRaster(rst)
			}
			return(rst)
		}
	}
)
