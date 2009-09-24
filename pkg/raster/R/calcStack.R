# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
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
	
	filename <- trim(filename)
	outraster <- raster(x, filename)
	dataType(outraster) <- datatype
	if (dataContent(x) == "all") {
		outraster <- setValues(outraster, apply(values(x), 1, fun)) 
		if (filename != "") {
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}
	} else {
		if (!canProcessInMemory(x, 4) & filename == '') {
			filename=rasterTmpFile()
			filename(outraster) <- filename
		}
		v <- vector(length=0)

		starttime <- proc.time()
		pb <- .setProgressBar(nrow(x), type=.progress(...))
		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			if (outraster@file@name == "") {
				v <- c(v, apply(values(x), 1, fun))
			} else {
				outraster <- setValues(outraster, apply(values(x), 1, fun), r) 
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}
			.doProgressBar(pb, r) 
		}
		.closeProgressBar(pb, starttime)
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}
	}		
	return(outraster)
}
)

