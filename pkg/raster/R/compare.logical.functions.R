# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


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


setMethod('==', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(cond)
	}
)	



setMethod('!=', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(!cond)
	}
)	


setMethod("Compare", signature(e1='RasterLayer', e2='numeric'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('second argument should be a single number')
		}
		if (.CanProcessInMemory(e1, 2)) {
			raster <- setRaster(e1)
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			raster <- setValues(raster, values=callGeneric(.getRasterValues(e1), rep(e2, ncell(e1)) ) )			
		} else {
			raster <- setRaster(e1, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			rowrep <- rep(e2, ncol(e1))
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e1, r), rowrep ), r)
				raster <- writeRaster(raster)
			}
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
			raster <- setRaster(e2)
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			raster <- setValues(raster, callGeneric(.getRasterValues(e2), rep(e1, ncell(e2)) ) )
		} else {
			raster <- setRaster(e2, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			rowrep <- rep(e1, ncol(e2))
			for (r in 1:nrow(e2)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e2, r), rowrep ), r)
				raster <- writeRaster(raster)
			}
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
			raster <- setRaster(e1) 
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			raster <- setValues(raster, callGeneric(.getRasterValues(e1), .getRasterValues(e2) ) ) 
		} else {
			raster <- setRaster(e1, filename=tempfile())
			raster <- setDatatype(raster, datatype='logical', datasize=2)
			for (r in 1:nrow(e1)) {
				raster <- setValues(raster, callGeneric( .getRowValues(e1, r), .getRowValues(e2, r) ), r)
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
				raster <- setRaster(e1)
				raster <- setDatatype(raster, datatype='logical', datasize=2)
				raster <- setValues(raster, callGeneric(.getLogicalValues(e1), .getLogicalValues(e2)))
			} else {
				raster <- setRaster(e1, filename=tempfile())
				raster <- setDatatype(raster, datatype='logical', datasize=2)	
				for (r in 1:nrow(e1)) {
					raster <- setValues(raster, callGeneric( .getLogicalRowValues(e1, r), .getLogicalRowValues(e2, r) ), r)
					raster <- writeRaster(raster)
				}
			}	
			return(raster)
		}	
	}
)



setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, datatype='logical', datasize=2)
		return(setValues(raster, is.na(.getRasterValues(x))))
	}
)	

setMethod("is.nan", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, datatype='logical', datasize=2)
		return(setValues(raster, is.nan(.getRasterValues(x))))
	}
)	

setMethod("is.infinite", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, datatype='logical', datasize=2)
		return(setValues(raster, values=is.infinite(.getRasterValues(x))))
	}
)	

setMethod("is.finite", signature(x='RasterLayer'),
	function(x) {
		raster <- setRaster(x)
		raster <- setDatatype(raster, datatype='logical', datasize=2)
		return(setValues(raster, values=is.finite(.getRasterValues(x))))
	}
)	

