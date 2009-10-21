# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


#mCalc <- function(...) { stop('mCalc has been replaced by generic function "calc"')}

setMethod('calc', signature(x='RasterStack', fun='function'), 

function(x, fun, filename="", ...) {

	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	outraster <- raster(x, filename)
	dataType(outraster) <- datatype

	if (!canProcessInMemory(x, 4) & filename == '') {
		filename=rasterTmpFile()
		filename(outraster) <- filename
	}
	v <- vector(length=0)

	starttime <- proc.time()
	pb <- pbSet(nrow(x), type=.progress(...))
	for (r in 1:nrow(x)) {
		sv <- apply(getValues(x, r), 1, fun)
		if (outraster@file@name == "") {
			v <- c(v, sv)
		} else {
			outraster <- setValues(outraster, sv, r) 
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}
		pbDo(pb, r) 
	}
	pbClose(pb, starttime)
	if (outraster@file@name == "") { 
		outraster <- setValues(outraster, v) 
	}
	return(outraster)
}
)

