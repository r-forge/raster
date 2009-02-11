# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


mCalc <- function(object, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {
	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}
	filename <- trim(filename)
	outraster <- setRaster(object, filename)
	outraster <- setDatatype(outraster, datatype) 
	v <- vector()
	for (r in 1:nrow(object)) {
		object <- readRow(object, r)
		vals <- apply(values(object), 1, fun)
		if (filename != "") {
			outraster <- setValues(outraster, vals, r) 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		} else {
			v <- c(v, vals)
		}
		if (r %in% track) {
			cat('row', r)
		}
	}	
	if (filename == "") {
		outraster <- setValues(outraster, v) 
	}
	return(outraster)
}

