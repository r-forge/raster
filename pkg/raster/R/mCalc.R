# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


mCalc <- function(object, fun=sum, filename="", overwrite=FALSE, asInt=FALSE) {
	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	outraster <- setRaster(object@layers[[1]], filename)
	if (filename(outraster)=="") {
		object <- readAll(object)
		outraster <- setValues(outraster, apply(values(object), 1, fun)) 
	} else {
		if (asInt) { 
			outraster <- setDatatype(outraster, "INT4S") 
		}
		for (r in 1:nrow(object)) {
			object <- readRow(object, r)
			vals <- apply(values(object), 1, fun)
			outraster <- setValues(outraster, vals, r) 
			outraster <- writeRaster(outraster, overwrite=overwrite)
		}
	}		
	return(outraster)
}

