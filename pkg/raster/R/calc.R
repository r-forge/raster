# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


calc <- function(raster, fun=sqrt, filename="", overwrite=FALSE, ForceIntOutput=FALSE) {
	if (length(fun(5)) > 1) { 
		stop("function 'fun' returns more than one value") 
	}
	filename <- trim(filename)
	outraster <- setRaster(raster, filename)
	if (ForceIntOutput) {setDatatype(outraster, 'integer')}
	
	if (!(dataContent(raster) == 'all' | dataContent(raster) == 'sparse' | dataSource(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
	if ( dataContent(raster) == 'all') {
		outraster <- setValues(outraster, fun(values(raster))) 
		if (filename(outraster)!="") { outraster <- writeRaster(outraster, overwrite=overwrite)
		}
	} else if ( dataContent(raster) == 'sparse') {
		outraster <- setValuesSparse(outraster, fun(values(raster)),  dataIndices(raster)) 
		if (filename(outraster) != "") { outraster <- writeRaster(outraster, overwrite=overwrite)
		}
	} else if (dataSource(raster) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(raster)))
			} else {
				outraster <- setValues(outraster, fun(values(raster)), r)
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}
		}
		if (filename(outraster) == "") { outraster <- setValues(outraster, v) }
	}
	return(outraster)
}



mCalc <- function(object, fun=sum, filename="", overwrite=FALSE, ForceIntOutput=FALSE) {
	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	outraster <- setRaster(object@rasters[[1]], filename)
	if (filename(outraster)=="") {
		object <- readAll(object)
		outraster <- setValues(outraster, apply(values(object), 1, fun)) 
	} else {
		if (ForceIntOutput) { 
			outraster <- setDatatype(outraster, "integer") 
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

