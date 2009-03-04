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
	filename <- trim(filename)
	outraster <- setRaster(x, filename)
	outraster <- setDatatype(outraster, datatype)
	
	if (!(dataContent(x) == 'all' | dataContent(x) == 'sparse' | dataSource(x) == 'disk')) {
		stop('RasterLayer has no data on disk, nor a complete set of values in memory')
	}
	
	if ( dataContent(x) == 'all') {
		outraster <- setValues(outraster, fun(values(x))) 
		if (filename(outraster)!="") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if ( dataContent(x) == 'sparse') {
		outraster <- setValuesSparse(outraster, fun(values(x)),  dataIndices(x)) 
		if (filename(outraster) != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if (dataSource(x) == 'disk') {
		if (!.CanProcessInMemory(x, 2) & filename == '') {
			filename <- tempfile()
			outraster <- setFilename(outraster, filename )
		}
		v <- vector(length=0)
		starttime <- proc.time()
		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(x)))
			} else {
				outraster <- setValues(outraster, fun(values(x)), r)
				outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
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
