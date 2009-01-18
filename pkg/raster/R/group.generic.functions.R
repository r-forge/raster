# Authors: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  January 2009
# Version 0,8
# Licence GPL v3


# for very large rasters, use 
#  filename <- tempfile() 



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
		rs <- setRaster(e1, values=callGeneric(.getRasterValues(e1), rep(e2, ncell(e1)) ) )
		rs <- setDatatype(rs, datatype='integer', datasize=2)
		return(rs)
	}
)	

setMethod("Compare", signature(e1='numeric', e2='RasterLayer'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('first argument should be a single number')
		}
		rs <- setRaster(e2, values=callGeneric(.getRasterValues(e2), rep(e1, ncell(e2)) ) ) 
		rs <- setDatatype(rs, datatype='integer', datasize=2)
		return(rs)
	}
)	

setMethod("Compare", signature(e1='RasterLayer', e2='RasterLayer'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare RasterLayers that have different BasicRaster attributes. See compare()")
		}	
		rs <- setRaster(e1, values=callGeneric(.getRasterValues(e1), .getRasterValues(e2) ) ) 
		rs <- setDatatype(rs, datatype='integer', datasize=2)
		return(rs)
	}
)	



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

.getLogicalValues <- function(x) {
	v <- .getRasterValues(x)
	v[v<0] <- 0
	v[v>0] <- 1
	return(v)
}

.getAllTypeOfValues <- function(x, y, i) {
	if ( (class(y) == 'RasterLayer' | class(y) == 'RasterStack' | class(y) == 'RasterBrick') & compare(c(x, y)) ) {			
		return(.getRasterValues(y))
	} else if (is.atomic(y)) {
		return(rep(y, ncell(x)))
	} else if (length(y)==ncell(x)) {
		return(y)
	} else {
		stop(paste("I do not understand argument",i + 1)) 
	}	
}

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
		return(setRaster(x, values=callGeneric(.getRasterValues(x))))
	}
)

setMethod("Logic", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			return(setRaster(e1, values=callGeneric(.getLogicalValues(e1), .getLogicalValues(e2))))
		}
	}
)


	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if (compare(c(e1, e2))) {
			return(setRaster(e1, values=callGeneric(.getRasterValues(e1), .getRasterValues(e2))))
		}	
	}
)

setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		return(setRaster(e1, values=callGeneric(.getRasterValues(e1), e2)))
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		return(setRaster(e2, values=callGeneric(.getRasterValues(e2), e1)))
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

