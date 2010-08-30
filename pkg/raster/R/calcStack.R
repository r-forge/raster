# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


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

	if (!canProcessInMemory(x, 2) & filename == '') {
		filename <- rasterTmpFile()
	} 
	
	if (filename == '') {
		v <- matrix(NA, nrow=ncol(outraster), ncol=nrow(outraster))
	} else {
		outraster <- writeStart(outraster, filename=filename, ...)
	}
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			


	if (class(fun) != 'character') {
		test <- try(slot(fun, 'generic')  == 'mean', silent=TRUE)
		if (isTRUE(test)) { fun <- 'mean' 
		} else {
			test <- try(deparse(fun)[[1]] == '.Primitive(\"sum\")', silent=TRUE)
			if (isTRUE(test)) { fun <- 'sum' }
		}
	}
	
	rowcalc <- FALSE
	if (class(fun) == 'character') {
		if (! fun %in% c('sum', 'mean') ) {
			stop("If 'fun' is a character variable, it should be either 'sum' or 'mean'")
		}
		rowcalc <- TRUE
	}
	
	if (class(fun) == 'character') {
		if (! fun %in% c('sum', 'mean') ) {
			stop("If 'fun' is a character variable, it should be either 'sum' or 'mean'")
		}
		#suggested by Matteo Mattiuzzi
		for (i in 1:tr$n) {
			if(fun == "mean" ) {
				sv <- rowMeans(getValues(x, row=tr$row[i], nrows=tr$nrows[i]), na.rm=na.rm)
			} else {
				sv <- rowSums(getValues(x, row=tr$row[i], nrows=tr$nrows[i]), na.rm=na.rm)
			}
			if (filename == "") {
				v[, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)] <- matrix(sv, nrow=ncol(outraster))
			} else {
				outraster <- writeValues(outraster, sv, tr$row[i])
			}
			pbStep(pb) 
		}
		
	} else {
		for (i in 1:tr$n) {
			sv <- apply(getValues(x, row=tr$row[i], nrows=tr$nrows[i]),  1,  fun, na.rm=na.rm)
			if (filename == "") {
				v[, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)] <- matrix(sv, nrow=ncol(outraster))
			} else {
				outraster <- writeValues(outraster, sv, tr$row[i])
			}
			pbStep(pb) 
		}
	}

	if (filename == "") { 	
		outraster <- setValues(outraster, as.vector(v))		
	} else {
		outraster <- writeStop(outraster)
	}

	pbClose(pb)
	return(outraster)
}
)

