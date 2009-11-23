# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("calc")) {
	setGeneric("calc", function(x, fun, ...)
		standardGeneric("calc"))
}	

setMethod('calc', signature(x='RasterLayer', fun='function'), 


function(x, fun, filename='', ...) {
	if (length(fun(5)) > 1) { 
		stop("function 'fun' returns more than one value") 
	}

	if (!(dataContent(x) == 'all' | dataSource(x) == 'disk')) {
		stop('RasterLayer has no data on disk, nor a complete set of values in memory')
	}

	filename <- trim(filename)
	outraster <- raster(x)

	if (dataSource(x) == 'disk') {
		if (!canProcessInMemory(x, 4) & filename == '') {
			filename <- rasterTmpFile()
		} else {
		#	if ( dataContent(x) != 'all') {
		#		x <- readAll(x)
		#	}
		}
	}
	
	if ( dataContent(x) == 'all' ) {
		outraster <- setValues(outraster, fun(values(x))) 
		if (filename != "") {
			outraster <- writeRaster(outraster, filename=filename, ...)
			outraster <- clearValues(outraster) 
		}
		return(outraster)
	} 
	
	if (filename == '') {
		v <- matrix(NA, ncol=nrow(outraster), nrow=ncol(outraster))
	} 
		
	pb <- pbCreate(nrow(x), type=.progress(...))
		
	for (r in 1:nrow(x)) {
		x <- readRow(x, r)
		if (filename == "") {
			v[,r] <- fun(values(x))
		} else {
			outraster <- setValues(outraster, fun(values(x)), r)
			outraster <- writeRaster(outraster, filename=filename, ...)
		}
		pbStep(pb, r)
	}
	pbClose(pb)
		
	if (filename == "") { 
		outraster <- setValues(outraster, as.vector(v)) 
	}
	return(outraster)
}
)
