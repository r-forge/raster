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
				filename <- rasterTmpFile()
				for (row in 1:nrow(e1)) {
					r <- setValues(r, callGeneric( as.numeric(getValues(e1, row)), getValues(e2, row) ), row)
					r <- writeRaster(r, filename=filename, doPB=TRUE)
				}
				if (getOption('verbose')) {
					cat('values were written to:', filename)
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
			filename <- rasterTmpFile()
			for (row in 1:nrow(e1)) {
				r <- setValues(r, callGeneric( as.numeric(getValues(e1, row)), e2) , row) 
				r <- writeRaster(r, filename=filename, doPB=TRUE)
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename)
			}			
			return(r)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
# simpler code, but would this make another copy of the objects?
#		callGeneric(e2, e1) 

		r <- raster(e2)
		if (canProcessInMemory(e2, 4)) {
			return( setValues(r, callGeneric(as.numeric(e1), getValues(e2))) )
		} else {
			filename <- rasterTmpFile()
			for (row in 1:nrow(e2)) {
				r <- setValues(r, callGeneric(as.numeric(e1), getValues(e2, row)) , row)
				r <- writeRaster(r, filename=filename, doPB=TRUE)
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename)
			}
			return(r)
		}		
	}
)



setMethod("Arith", signature(e1='RasterBrick', e2='numeric'),
    function(e1, e2){ 
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(e1,  callGeneric(getValues(e1), e2) ) )
		} else {
			r <- brick(e1)
			filename <- rasterTmpFile()
			for (row in 1:nrow(e1)) {
				r <- setValues(r, callGeneric( getValues(e1, row), e2) , row) 
				r <- writeRaster(r, filename=filename, doPB=TRUE)
			}
			if (getOption('verbose')) {
				cat('values were written to:', filename)
			}			
			return(r)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterBrick'),
    function(e1, e2){ 
		callGeneric(e2, e1) 
	}
)




setMethod("Arith", signature(e1='Extent', e2='numeric'),
	function(e1, e2){ 
		r <- e1@xmax - e1@xmin
		d <- callGeneric(r, e2)
		d <- (d - r) / 2
		e1@xmax <- e1@xmax + d
		e1@xmin <- e1@xmin - d
		r <- e1@ymax - e1@ymin
		d <- callGeneric(r, e2)
		d <- (d - r) / 2
		e1@ymax <- e1@ymax + d
		e1@ymin <- e1@ymin - d
		return(e1)
	}
)

setMethod("Arith", signature(e1='numeric', e2='Extent'),
    function(e1, e2){ 
		callGeneric(e2,e1)
	}
)

