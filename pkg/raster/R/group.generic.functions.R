# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.CanProcessInMemory <- function(raster, n=2, datasize=16) {
	memneed <- 1.1 * n * ncell(raster) * datasize / 1048576
	if (memneed > memory.size(max = T)) {
		return( FALSE )
	} else { 
		return( TRUE ) 
	}
}


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
		if (.CanProcessInMemory(x, 1)) {
			raster <- setRaster(x, values=callGeneric(.getRasterValues(x)))
		} else {
			raster <- setRaster(x, filename=tempfile())
			for (r in 1:nrow(x)) {
				raster <- setValues(raster, callGeneric( .getRowValues(x, r) ), r)
				raster <- writeRaster(raster)
			}
		}
		return(raster)
	}
)



	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			if (.CanProcessInMemory(e1, 2)) {
				raster <- setRaster(e1, values=callGeneric(.getRasterValues(e1), .getRasterValues(e2)))
			} else {
				raster <- setRaster(e1, filename=tempfile())
				for (r in 1:nrow(e1)) {
					raster <- setValues(raster, callGeneric( .getRowValues(e1, r), .getRowValues(e2, r) ), r)
					raster <- writeRaster(raster)
				}
			}	
			return(raster)
		}	
	}
)


setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		if (.CanProcessInMemory(e1, 2)) {
			return(setRaster(e1, values=callGeneric(.getRasterValues(e1), e2)))
		} else {
			raster <- setRaster(e1, filename=tempfile())
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e1, r), e2) , r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		if (.CanProcessInMemory(e2, 2)) {
			return(setRaster(e2, values=callGeneric(.getRasterValues(e2), e1)))
		} else {
			raster <- setRaster(e2, filename=tempfile())
			for (r in 1:nrow(e2)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e2, r), e1) , r)
				raster <- writeRaster(raster)
			}
			return(raster)
		}		
	}
)


if (!isGeneric("median")) {
	setGeneric("median", function(x, na.rm=FALSE)
		standardGeneric("median"))
}


setMethod('median', signature(x='Raster'), 
	function(x, na.rm=FALSE){
		if (dataContent(x) == 'all') {
			return(median(values(x), na.rm=na.rm))
		} else {
# needs to be improved for large files. Make frequency table row by row.....
			return(median(values(readAll(x)), na.rm=na.rm))
		}
	}
)





if (!isGeneric("rmedian")) {
	setGeneric("rmedian", function(x, ..., na.rm=FALSE)
		standardGeneric("rmedian"))
}

setMethod('rmedian', signature(x='Raster'), 
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, median, na.rm=na.rm)))
		} else {
			stk <- stack(c(x,obs))
			v <- vector()
			for (r in 1:nrow(stk)) {
				v <- c(v, apply(values(readRow(stk, r)), 1, median, na.rm=na.rm)) 
			}
			return(setRaster(x, values=v))
		}
	}
)



setMethod("max", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, max, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, max, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("min", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, min, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, min, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("sum", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowSums(as.matrix(.getRasterValues(x)), na.rm)))
		} else {
			v <- .getRasterValues(x)
			if (!(is.null(dim(v)))) {
				v <- rowSums(as.matrix(.getRasterValues(x)), na.rm=na.rm)
			} 
			for (i in 1:length(obs)) {
				vv <- .getAllTypeOfValues(x, obs[[i]], i)
				v <- rowSums(cbind(v, vv), na.rm=na.rm)
			}
		return(setRaster(x, values=v))
		}
	}
)


#todo "any", "all" 

	
setMethod("range", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		return(max(x, ..., na.rm=na.rm) - min(x, ..., na.rm=na.rm))
	}
)	


