# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("Math", signature(x='Raster'),
    function(x){ 
		stop('This function has not been defined for the class of this object')
	}
)
setMethod("Math2", signature(x='Raster'),
    function(x, digits=0){ 
		stop('This function has not been defined for the class of this object')
	}
)


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		fname <- as.character(sys.call(sys.parent())[[1]])
		rst <- raster(x)
		if (canProcessInMemory(rst, 3)) {
			rst <- setValues(rst, callGeneric(getValues(x)))
		} else {
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				datatype <- 'INT4S'
			} else {
				datatype <- .datatype()
			}
			filename <- rasterTmpFile() 
			pb <- pbCreate(nrow(rst), type=.progress())			
			for (r in 1:nrow(rst)) {
				rst <- setValues(rst, callGeneric( getValues(x, r) ), r)
				rst <- writeRaster(rst, filename=filename, datatype=datatype)
				pbStep(pb, rst) 
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename)
			}
		}
		return(rst)
	}
)


setMethod("Math2", signature(x='RasterLayer'), 
	function (x, digits=0) {
		digits <- max(0, digits)
		rst <- raster(x)
		if (canProcessInMemory(rst, 3)) {
			rst <- setValues(rst, callGeneric( getValues(x), digits))
			return(rst)
		} else {
			if (digits == 0) {
				datatype <- 'INT4S'
			} else {
				datatype <- .datatype()
			}
			filename <- rasterTmpFile() 			
			pb <- pbCreate(nrow(x), type=.progress() )
			for (r in 1:nrow(x)) {
				rst <- setValues(rst, callGeneric(getValues(x, r), digits), r)
				rst <- writeRaster(rst, filename=filename, datatype=datatype)
				pbStep(pb, r) 
				
			}
			return(rst)
		}
	}
)
