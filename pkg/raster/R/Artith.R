# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			r <- raster(e1)
			if (canProcessInMemory(e1, 4)) {
				return( setValues(r, values=callGeneric( as.numeric(getValues(e1)), getValues(e2))) )
			} else {
				dataType(r) <- .datatype()
				filetype <- .filetype()
				overwrite <- .overwrite()

				filename(r) <- rasterTmpFile() 
				
				starttime <- proc.time()
				pb <- .setProgressBar(nrow(e1), type=.progress() )
				for (row in 1:nrow(e1)) {
					r <- setValues(r, callGeneric( as.numeric(getValues(e1, row)), getValues(e2, row) ), row)
					r <- writeRaster(r, filetype=filetype, overwrite=overwrite)
					.doProgressBar(pb, row) 
				}
				.closeProgressBar(pb, starttime)
				if (getOption('verbose')) {
					cat('values were written to:', raster@file@name)
				}
				return(r)
			}
		}	
	}
)


setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		r <- raster(e1)
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(r,  callGeneric(as.numeric(getValues(e1)), e2) ) )
		} else {
			dataType(r) <- .datatype()
			filetype <- .filetype()
			overwrite <- .overwrite()
			
			filename(r) <- rasterTmpFile() 
			
			starttime <- proc.time()
			pb <- .setProgressBar(nrow(e1), type=.progress())
			for (row in 1:nrow(e1)) {
				r <- setValues(r, callGeneric( as.numeric(getValues(e1, row)), e2) , row) 
				r <- writeRaster(r, filetype=filetype, overwrite=overwrite)
				.doProgressBar(pb, row) 
			}
			.closeProgressBar(pb, starttime)
			if (getOption('verbose')) {
				cat('values were written to:', filename(raster))
			}			
			return(r)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		r <- raster(e2)
		if (canProcessInMemory(e2, 4)) {
			return( setValues(r, callGeneric(as.numeric(e1), getValues(e2))) )
		} else {
			dataType(r) <- .datatype()
			filetype <- .filetype()
			overwrite <- .overwrite()
			progress <- .progress()
			filename(r) <- rasterTmpFile() 
			starttime <- proc.time()
			pb <- .setProgressBar(nrow(e1), type=progress)
			for (row in 1:nrow(e2)) {
				r <- setValues(r, callGeneric(as.numeric(e1), getValues(e2, row)) , row)
				r <- writeRaster(r, filetype=filetype, overwrite=overwrite)
				.doProgressBar(pb, row) 
			}
			.closeProgressBar(pb, starttime)
			if (getOption('verbose')) {
				cat('values were written to:', filename(raster))
			}
			return(r)
		}		
	}
)

