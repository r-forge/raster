# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

if (!isGeneric("calc")) {
	setGeneric("calc", function(x, fun, ...)
		standardGeneric("calc"))
}	

setMethod('calc', signature(x='RasterLayer', fun='function'), 

function(x, fun, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {
	if (length(fun(5)) > 1) { 
		stop("function 'fun' returns more than one value") 
	}
	
	outraster <- raster(x, filename)
	dataType(outraster) <- datatype
	
	if (!(dataContent(x) == 'all' | dataContent(x) == 'sparse' | dataSource(x) == 'disk')) {
		stop('RasterLayer has no data on disk, nor a complete set of values in memory')
	}
	
	if ( dataContent(x) == 'all') {
		outraster <- setValues(outraster, fun(values(x))) 
		if (outraster@file@name != "") {
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if ( dataContent(x) == 'sparse') {
		outraster <- setValuesSparse(outraster, fun(values(x)),  dataIndices(x)) 
		if (outraster@file@name != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if (dataSource(x) == 'disk') {
		if (!canProcessInMemory(x, 3) & filename == '') {
			filename <- tempfile()
			filename(outraster) <- filename
		}
		v <- vector(length=0)
		starttime <- proc.time()

		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			if (outraster@file@name == "") {
				v <- c(v, fun(values(x)))
			} else {
				outraster <- setValues(outraster, fun(values(x)), r)
				outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
			}
			
		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
			
		}
		if (outraster@file@name == "") { 
			outraster <- setValues(outraster, v) 
		}
	}
	return(outraster)
}
)
