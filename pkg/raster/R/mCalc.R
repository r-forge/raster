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
	outraster <- setRaster(object@layers[[1]], filename)
	outraster <- setDatatype(outraster, datatype)
	if (dataContent(object) == "all") {
		outraster <- setValues(outraster, apply(values(object), 1, fun)) 
		if (filename != "") {
			outRaster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}
	} else {
		for (r in 1:nrow(object)) {
			object <- readRow(object, r)
			vals <- apply(values(object), 1, fun)
			outraster <- setValues(outraster, vals, r) 
			outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			if (r %in% track) {
				cat('row', r)
			}
		}
	}		
	return(outraster)
}

