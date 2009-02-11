# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3




calc <- function(raster, fun=sqrt, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S') {
	if (length(fun(5)) > 1) { 
		stop("function 'fun' returns more than one value") 
	}
	filename <- trim(filename)
	outraster <- setRaster(raster, filename)
	
	outraster <- setDatatype(outraster, datatype)
	
	if (!(dataContent(raster) == 'all' | dataContent(raster) == 'sparse' | dataSource(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
	if ( dataContent(raster) == 'all') {
		outraster <- setValues(outraster, fun(values(raster))) 
		if (filename(outraster)!="") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if ( dataContent(raster) == 'sparse') {
		outraster <- setValuesSparse(outraster, fun(values(raster)),  dataIndices(raster)) 
		if (filename(outraster) != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
	} else if (dataSource(raster) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(raster)))
			} else {
				outraster <- setValues(outraster, fun(values(raster)), r)
				outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype)
			}
		}
		if (filename(outraster) == "") { outraster <- setValues(outraster, v) }
	}
	return(outraster)
}


