# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.getLogicalRowValues <- function(x, r) {
# need to take care of 'spase'
	v <- .getRowValues(x, r)
	v[v!=0] <- 1
	return(v)
}	


.getLogicalValues <- function(x) {
	v <- .getRasterValues(x)
	v[v!=0] <- 1
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



setMethod('!', signature(x='RasterLayer'),
	function(x){
		if (canProcessInMemory(x, 3)) {
			return(setValues(x, !values(x)))
		} else {
			rst <- raster(x, filename=tempfile())
			dataType(rst) <- 'LOG1S'
			for (r in 1:nrow(x)) {
				rst <- setValues(rst, !.getRowValues(x, r), r)
				writeRaster(rst)
			}
			return(rst)		
		}
	}
)	



setMethod("Compare", signature(e1='RasterLayer', e2='numeric'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('second argument should be a single number')
		}
		rst <- raster(e1)
		rst <- setDatatype(rst, 'LOG1S')
		if (canProcessInMemory(e1, 3)) {
			rst <- setValues(rst, values=callGeneric(.getRasterValues(e1), rep(e2, ncell(e1)) ) )			
		} else {
			filename(rst) <- tempfile()
			rowrep <- rep(e2, ncol(e1))
			for (r in 1:nrow(e1)) {
				rst <- setValues(rst, callGeneric( .getRowValues(e1, r), rowrep ), r)
				writeRaster(rst)
			}
		}
		return(rst)
	}
)	



setMethod("Compare", signature(e1='numeric', e2='RasterLayer'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e1) & length(e1)==1)) {
			stop('first argument should be a single number')
		}
		if (canProcessInMemory(e2, 3)) {
			rst <- raster(e2)
			dataType(rst) <- 'LOG1S'
			rst <- setValues(rst, callGeneric(.getRasterValues(e2), rep(e1, ncell(e2)) ) )
		} else {
			rst <- raster(e2, filename=tempfile())
			dataType(rst) <- 'LOG1S'
			rowrep <- rep(e1, ncol(e2))
			for (r in 1:nrow(e2)) {
				rst <- setValues(rst, callGeneric( .getRowValues(e2, r), rowrep ), r)
				writeRaster(rst)
			}
		}
		return(rst)
	}
)	

setMethod("Compare", signature(e1='RasterLayer', e2='RasterLayer'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare RasterLayers that have different BasicRaster attributes. See compare()")
		}	
		rst <- raster(e1) 
		dataType(rst) <- 'LOG1S'
		if (canProcessInMemory(e1, 3)) {
			rst <- setValues(rst, callGeneric(.getRasterValues(e1), .getRasterValues(e2) ) ) 
		} else {
			filename(rst) <- tempfile()
			for (r in 1:nrow(e1)) {
				rst <- setValues(rst, callGeneric( .getRowValues(e1, r), .getRowValues(e2, r) ), r)
				writeRaster(rst)
			}
		}
		return(rst)
	}
)	





setMethod("Logic", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			rst <- raster(e1)
			rst <- setDatatype(rst, 'LOG1S')
			if (canProcessInMemory(e1, 3)) {
				rst <- setValues(rst, callGeneric(.getLogicalValues(e1), .getLogicalValues(e2)))
			} else {
				filename(rst) <- tempfile()
				for (r in 1:nrow(e1)) {
					rst <- setValues(rst, callGeneric( .getLogicalRowValues(e1, r), .getLogicalRowValues(e2, r) ), r)
					writeRaster(rst)
				}
			}	
			return(rst)
		}	
	}
)


