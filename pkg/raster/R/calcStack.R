# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


#mCalc <- function(...) { stop('mCalc has been replaced by generic function "calc"')}

setMethod('calc', signature(x='RasterStack', fun='function'), 

function(x, fun, ...) {

	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	filename <- .filename(...)
	outraster <- raster(x, filename)
	datatype <- .datatype(...)
	.setDataType(outraster) <- datatype

	if (!canProcessInMemory(x, 4) & filename == '') {
		filename <- rasterTmpFile()
		.setFilename(outraster) <- filename
	}
	v <- vector(length=0)

	
	pb <- pbCreate(nrow(x), type=.progress(...))
	for (r in 1:nrow(x)) {
		sv <- apply(getValues(x, r), 1, fun)
		if (outraster@file@name == "") {
			v <- c(v, sv)
		} else {
			outraster <- setValues(outraster, sv, r) 
			outraster <- writeRaster(outraster, filename=filename, ...)
		}
		pbStep(pb, r) 
	}
	pbClose(pb)
	if (outraster@file@name == "") { 
		outraster <- setValues(outraster, v) 
	}
	return(outraster)
}
)

