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
	outraster <- setRaster(x, filename)
	outraster <- setDatatype(outraster, datatype)
	if (dataContent(x) == "all") {
		outraster <- setValues(outraster, apply(values(x), 1, fun)) 
		if (filename != "") {
			outRaster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
		}
	} else {
		starttime <- proc.time()
		if (!.CanProcessInMemory(x, 1) & filename == '') {
			filename=tempfile()
			outraster <- setFilename(outraster, filename )
		}
		v <- vector(length=0)
		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			if (filename(outraster)=="") {
				v <- c(v, apply(values(x), 1, fun))
			} else {
				outraster <- setValues(outraster, apply(values(x), 1, fun), r) 
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}
	
			if (r %in% track) {
				elapsed <- (proc.time() - starttime)[3]
				tpr <- elapsed /r
				ttg <- round(tpr/60 * (nrow(x) - r), digits=1)
				cat('row', r, '-', ttg, 'minutes to go\n')
			}
		
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	}		
	return(outraster)
}
)

