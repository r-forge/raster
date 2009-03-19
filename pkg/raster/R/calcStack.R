# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


#mCalc <- function(...) { stop('mCalc has been replaced by generic function "calc"')}

setMethod('calc', signature(x='RasterStack', fun='function'), 

function(x, fun, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {

	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	filename <- trim(filename)
	outraster <- raster(x, filename)
	outraster <- setDatatype(outraster, datatype)
	if (dataContent(x) == "all") {
		outraster <- setValues(outraster, apply(values(x), 1, fun)) 
		if (filename != "") {
			writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}
	} else {
		starttime <- proc.time()
		if (!canProcessInMemory(x, 4) & filename == '') {
			filename=tempfile()
			filename(outraster) <- filename
		}
		v <- vector(length=0)
		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			if (outraster@file@name == "") {
				v <- c(v, apply(values(x), 1, fun))
			} else {
				outraster <- setValues(outraster, apply(values(x), 1, fun), r) 
				writeRaster(outraster, filetype=filetype, overwrite=overwrite)
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

