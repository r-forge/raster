# Authors: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  January 2009
# Version 0,8
# Licence GPL v3


# for very large rasters, use 
#  filename <- tempfile() 


.CanProcessInMemory <- function(raster, n=2, datasize=16) {
#	memalloc <- n * ncell(raster) * 8
#	memavailable <- memory.limit()-memory.size()
# For now something simplistic :
	maxalloc <- 10^8
	if ( (ncell(raster) * n * datasize) > maxalloc ) { 
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


.getLogicalRowValues <- function(x, r) {
# need to take care of 'spase'
	v <- .getRowValues(x, r)
	v[v<0] <- 0
	v[v>0] <- 1
	return(v)
}	


.getLogicalValues <- function(x) {
	v <- .getRasterValues(x)
	v[v<0] <- 0
	v[v>0] <- 1
	return(v)
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


setMethod('==', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		return(cond)
	}
)	



setMethod('!=', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		return(!cond)
	}
)	


setMethod("Compare", signature(e1='RasterLayer', e2='numeric'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('second argument should be a single number')
		}
		if (.CanProcessInMemory(e1, 2)) {
			raster <- setRaster(e1, values=callGeneric(.getRasterValues(e1), rep(e2, ncell(e1)) ) )
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		} else {
			raster <- setRaster(e1, filename=tempfile())
			rowrep <- rep(e2, ncol(e1))
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e1, r), rowrep ), r)
				raster <- writeRaster(raster)
			}
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		}
		return(raster)
	}
)	

setMethod("Compare", signature(e1='numeric', e2='RasterLayer'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('first argument should be a single number')
		}
		if (.CanProcessInMemory(e2, 2)) {
			raster <- setRaster(e2, values=callGeneric(.getRasterValues(e2), rep(e1, ncell(e2)) ) )
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		} else {
			raster <- setRaster(e2, filename=tempfile())
			rowrep <- rep(e1, ncol(e2))
			for (r in 1:nrow(e2)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e2, r), rowrep ), r)
				raster <- writeRaster(raster)
			}
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		}
		return(raster)
	}
)	

setMethod("Compare", signature(e1='RasterLayer', e2='RasterLayer'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare RasterLayers that have different BasicRaster attributes. See compare()")
		}	
		if (.CanProcessInMemory(e1, 2)) {
			raster <- setRaster(e1, values=callGeneric(.getRasterValues(e1), .getRasterValues(e2) ) ) 
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		} else {
			raster <- setRaster(e1, filename=tempfile())
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e1, r), .getRowValues(e2, r) ), r)
				raster <- writeRaster(raster)
			}
			raster <- setDatatype(raster, datatype='integer', datasize=2)
		}
		return(raster)
	}
)	



setMethod("[", "RasterLayer",
	function(x, i, j, ..., drop = TRUE) {
		if (!missing(drop)) { stop("drop is ignored. It is always set to FALSE") }
		if (!missing(j)) { stop("can only set values with a single index (a vector)") }
		if (missing(i)) { return(x) }
		v <- values(i)
		v[x] <- i
		return(setRaster(x, v))
	}
)


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



setMethod("Logic", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			if (.CanProcessInMemory(e1, 2)) {
				raster <- setRaster(e1, values=callGeneric(.getLogicalValues(e1), .getLogicalValues(e2)))
			} else {
				raster <- setRaster(e1, filename=tempfile())
				for (r in 1:nrow(e1)) {
					raster <- setValues(raster, callGeneric( .getLogicalRowValues(e1, r), .getLogicalRowValues(e2, r) ), r)
					raster <- writeRaster(raster)
				}
			}	
			return(raster)
		}	
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

setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.na(.getRasterValues(x))))
	}
)	

setMethod("is.nan", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.nan(.getRasterValues(x))))
	}
)	

setMethod("is.infinite", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.infinite(.getRasterValues(x))))
	}
)	

setMethod("is.finite", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.finite(.getRasterValues(x))))
	}
)	

