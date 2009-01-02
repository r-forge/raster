# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


calc <- function(object, fun, filename="", overwrite=FALSE, ForceIntOutput=FALSE) {
	if (class(object) == "RasterLayer") { 
		.singleLayerCalc(object, fun, filename, overwrite, ForceIntOutput) 
	} else {
		.multipleLayersCalc(object, fun, filename, overwrite, ForceIntOutput) 
	}
}


.singleLayerCalc <- function(raster, fun, filename, overwrite, ForceIntOutput) {
	if (length(fun(5)) > 1) { 
		stop("function 'fun' returns more than one value") 
	}
	outraster <- setRaster(raster, filename)
	if (ForceIntOutput) {setDatatype(outraster, 'integer')}
	
	if (!(dataContent(raster) == 'all' | dataContent(raster) == 'sparse' | dataSource(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
	if ( dataContent(raster) == 'all') {
		outraster <- setValues(outraster, fun(values(raster))) 
		if (filename(outraster)!="") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if ( dataContent(raster) == 'sparse') {
		outraster <- setValuesSparse(outraster, fun(values(raster)),  dataIndices(raster)) 
		if (filename(outraster) != "") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if (dataSource(raster) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(raster)))
			} else {
				outraster <- setValuesRow(outraster, fun(values(raster)), r)
				outraster <- write.row(outraster, overwrite=overwrite)
			}
		}
		if (filename(outraster) == "") { outraster <- setValues(outraster, v) }
	}
	return(outraster)
}



.multipleLayersCalc <- function(rstack, fun, filename, overwrite, ForceIntOutput) {
	if (length(fun(seq(1:5))) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	outraster <- setRaster(rstack@rasters[[1]], filename)
	if (filename(outraster)=="") {
		rstack <- .rasterstack.read.all(rstack)
		outraster <- setValues(outraster, apply(values(rstack), 1, fun)) 
	} else {
		if (ForceIntOutput) { outraster <- setDatatype(outraster, "integer") }
		for (r in 1:nrow(rstack)) {
			rstack <- readRow(rstack, r)
			vals <- apply(values(rstack), 1, fun)
			outraster <- setValuesRow(outraster, vals, r) 
			outraster <- write.row(outraster, overwrite)
		}
	}		
	return(outraster)
}

