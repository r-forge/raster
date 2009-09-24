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
		if (fname %in% c('floor', 'ceiling', 'trunc')) {
			dataType(rst) <- 'INT4S'
		} else {
			dataType(rst) <- .datatype()
		}

		if (canProcessInMemory(rst, 3)) {
			rst <- setValues(rst, callGeneric(getValues(x)))
		} else {
			filetype <- .filetype()
			overwrite <- .overwrite()
		
			filename(rst) <- .filename()
			if (filename(rst) == "") { 
				filename(rst) <- rasterTmpFile() 
			}

			starttime <- proc.time()
			pb <- .setProgressBar(nrow(rst), type=.progress())			
			for (r in 1:nrow(rst)) {
				rst <- setValues(rst, callGeneric( getValues(x, r) ), r)
				rst <- writeRaster(rst, filetype=filetype, overwrite=overwrite)
				.doProgressBar(pb, rst) 
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
		if (digits == 0) {
			dataType(rst) <- 'INT4S'
		} else {
			dataType(rst) <- .datatype()
		}
		if (canProcessInMemory(rst, 3)) {
			rst <- setValues(rst, callGeneric( getValues(x), digits))
			return(rst)
		} else {
			filetype <- .filetype()
			overwrite <- .overwrite()

			filename(rst) <- .filename()
			if (filename(rst) == "") { 
				filename(rst) <- rasterTmpFile() 
			}
			
			starttime <- proc.time()
			pb <- .setProgressBar(nrow(x), type=.progress() )
			for (r in 1:nrow(x)) {
				rst <- setValues(rst, callGeneric(getValues(x, r), digits), r)
				rst <- writeRaster(rst, filetype=filetype, overwrite=overwrite)
				.doProgressBar(pb, r) 
				
			}
			return(rst)
		}
	}
)
