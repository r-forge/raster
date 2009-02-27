# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



.getRasterValues <- function(x) {
# need to take care of 'spase'
	if (dataContent(x) != 'all') {
		if (class(x) == "RasterLayer") {
			if (dataSource(x) == 'ram') {
				stop('no data on disk or in memory')
			} else x <- readAll(x)	
		} else {
			x <- readAll(x)
		}
	}
	return(values(x))
}	

.getRowValues <- function(x, r) {
# need to take care of 'spase'
	if (dataContent(x) == 'all') {
		return(valuesRow(x, r))
	} else {	
		if (dataSource(x) == 'disk') {
			return(values(readRow(x, r)))
		} else { 
			stop('data not on disk or in memory')
		} 
	}	
}	


.getAllTypeOfValues <- function(x, y, i) {
	if ( extends(class(y), "Raster") & compare(c(x, y)) ) {			
		return(.getRasterValues(y))
	} else if (is.atomic(y)) {
		return(rep(y, ncell(x)))
	} else if (length(y)==ncell(x)) {
		return(y)
	} else {
		stop(paste("I do not understand argument",i + 1)) 
	}	
}




setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		if (.CanProcessInMemory(x, 3)) {
			raster <- setRaster(x, values=callGeneric(.getRasterValues(x)))
		} else {
			raster <- setRaster(x, filename=tempfile())
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



	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			if (.CanProcessInMemory(e1, 4)) {
				raster <- setRaster(e1, values=callGeneric( as.numeric(.getRasterValues(e1)), .getRasterValues(e2)))
			} else {
				raster <- setRaster(e1, filename=tempfile())
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
		if (.CanProcessInMemory(e1, 3)) {
			return(setRaster(e1, values=callGeneric(as.numeric(.getRasterValues(e1)), e2) ) )
		} else {
			raster <- setRaster(e1, filename=tempfile())
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
		if (.CanProcessInMemory(e2, 3)) {
			return(setRaster(e2, values=callGeneric(as.numeric(e1), .getRasterValues(e2))))
		} else {
			raster <- setRaster(e2, filename=tempfile())
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

