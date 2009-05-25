# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			r <- raster(e1)
			if (canProcessInMemory(e1, 4)) {
				return( setValues(r, values=callGeneric( as.numeric(.getRasterValues(e1)), .getRasterValues(e2))) )
			} else {
				filename(r) <- rasterTmpFile()
				for (row in 1:nrow(e1)) {
					r <- setValues(r, callGeneric( as.numeric(.getRowValues(e1, row)), .getRowValues(e2, row) ), row)
					r <- writeRaster(r)
				}
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
			return ( setValues(r,  callGeneric(as.numeric(.getRasterValues(e1)), e2) ) )
		} else {
			filename(r) <- rasterTmpFile()
			for (row in 1:nrow(e1)) {
				r <- setValues(r, callGeneric( as.numeric(.getRowValues(e1, row)), e2) , row) 
				r <- writeRaster(r)
			}
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
			return( setValues(r, callGeneric(as.numeric(e1), .getRasterValues(e2))) )
		} else {
			filename(r) <- rasterTmpFile()
			for (row in 1:nrow(e2)) {
				r <- setValues(r, callGeneric(as.numeric(e1), .getRowValues(e2, row)) , row)
				r <- writeRaster(r)
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename(raster))
			}
			return(r)
		}		
	}
)

