# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			if (canProcessInMemory(e1, 4)) {
				raster <- raster(e1, values=callGeneric( as.numeric(.getRasterValues(e1)), .getRasterValues(e2)))
			} else {
				raster <- raster(e1, filename=tempfile())
				for (r in 1:nrow(e1)) {
					raster <- setValues(raster, callGeneric( as.numeric(.getRowValues(e1, r)), .getRowValues(e2, r) ), r)
					raster <- writeRaster(raster)
				}
				if (options('verbose')[[1]]) {
					cat('values were written to:', filename(raster))
				}
			}	
			return(raster)
		}	
	}
)


setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		if (canProcessInMemory(e1, 4)) {
			return(raster(e1, values=callGeneric(as.numeric(.getRasterValues(e1)), e2) ) )
		} else {
			raster <- raster(e1, filename=tempfile())
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( as.numeric(.getRowValues(e1, r)), e2) , r) 
				raster <- writeRaster(raster)
			}
			if (options('verbose')[[1]]) {
				cat('values were written to:', filename(raster))
			}			
			return(raster)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		if (canProcessInMemory(e2, 4)) {
			return(raster(e2, values=callGeneric(as.numeric(e1), .getRasterValues(e2))))
		} else {
			raster <- raster(e2, filename=tempfile())
			for (r in 1:nrow(e2)) {
				raster <- setValues(raster, callGeneric(as.numeric(e1), .getRowValues(e2, r)) , r)
				raster <- writeRaster(raster)
			}
			if (options('verbose')[[1]]) {
				cat('values were written to:', filename(raster))
			}
			return(raster)
		}		
	}
)

