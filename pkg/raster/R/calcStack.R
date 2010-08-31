# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.makeTextFun <- function(fun, names=c('mean', 'sum')) {
	if (class(fun) != 'character') {
		if ('mean' %in% names) {
			test <- try(slot(fun, 'generic')  == 'mean', silent=TRUE)
			if (isTRUE(test)) { fun <- 'mean' }
		}
		test <- try(deparse(fun)[[1]], silent=TRUE)
		if ('sum' %in% names) {
			if (test == '.Primitive(\"sum\")') { fun <- 'sum' }
		}
		if ('min' %in% names) {
			if (test == '.Primitive(\"min\")') { fun <- 'min' }
		}
		if ('max' %in% names) {
			if (test == '.Primitive(\"max\")') { fun <- 'max' }
		}
	}
	if (class(fun) == 'character') {
		if (! fun %in% names ) {
			stop("If 'fun' is a character variable, it should be one of: ", names)
		}
	}
	return(fun)
}


setMethod('calc', signature(x='RasterStackBrick', fun='ANY'), 
function(x, fun, filename='', na.rm=TRUE, ...) {

	nl <- nlayers(x)
	test <- length(fun(1:nl))
	if (test != 1) {
		if (test == nl) {
			return( .calcLayers(x, fun, filename, ...) )
		} else {
			stop("'fun' does not return the correct number of values. It should be 1 or nlayers(x)") 
		}
	}
	test <- try(fun(1:nl, na.rm=TRUE), silent=TRUE)
	if (class(test) == 'try-error') {
		stop("'fun' does take an 'na.rm' arugment. Add na.rm or dots (...) to the function arguments") 
	}
	
	filename <- trim(filename)
	outraster <- raster(x)

	fun <- .makeTextFun(fun, c('mean', 'sum', 'min', 'max'))
	if (class(fun) == 'character') { rowcalc <- TRUE } else { rowcalc <- FALSE }
	
	if (canProcessInMemory(x, 2)) {
		x <- getValues(x)
		if (class(fun) == 'character') { #suggested by Matteo Mattiuzzi
			if(fun == 'mean' ) {
				x <- rowMeans(x, na.rm=na.rm )
			} else if (fun == 'sum') {
				x <- rowSums(x, na.rm=na.rm )
			} else if (fun == 'min') {
				x <- .rowMin(x, na.rm=na.rm )
			} else if (fun == 'max') {
				x <- .rowMax(x, na.rm=na.rm )
			}
		} else {
			x <- apply(x, 1, fun, na.rm=na.rm)
		}
		x <- setValues(outraster, x)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return ( x)		
	} 

# else 
	
	if (filename == '') { filename <- rasterTmpFile()	} 
	
	outraster <- writeStart(outraster, filename=filename, ...)
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			

	if (class(fun) == 'character') {
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if(fun == 'mean' ) {
				v <- rowMeans(v, na.rm=na.rm )
			} else if ( fun == 'sum' ) {
				v <- rowSums(v, na.rm=na.rm )
			} else if ( fun == 'min') {
				v <- .rowMin(v, na.rm=na.rm )
			} else if (fun == 'max' ) {
				v <- .rowMax(v, na.rm=na.rm )
			}
			outraster <- writeValues(outraster, v, tr$row[i])
			pbStep(pb) 
		}
		
	} else {
		for (i in 1:tr$n) {
			sv <- apply(getValues(x, row=tr$row[i], nrows=tr$nrows[i]),  1,  fun, na.rm=na.rm)
			outraster <- writeValues(outraster, sv, tr$row[i])
			pbStep(pb) 
		}
	}
	outraster <- writeStop(outraster)
	pbClose(pb)
	return(outraster)
}
)

