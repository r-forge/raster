# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


if (!isGeneric("cover")) {
	setGeneric("cover", function(x,y,...)
		standardGeneric("cover"))
}	

setMethod('cover', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, filename="", overwrite=TRUE, filetype='raster', datatype=dataType(x), track=-1) {
	
	compare(c(x, y))
	
	outRaster <- raster(x, filename)
	outRaster <- setDatatype(outRaster, datatype)
	
	# need to check the datatype. if x = INT and y = FLT, result should be FLT?
	
	if ( dataContent(x) == 'all' & dataContent(y) == 'all') {
		x@data@values[is.na(x@data@values)] <- values(y)[is.na(x@data@values)]
		rm(y)
		outRaster <- setValues(outRaster, values(x))
		if (filename(outRaster) != "") { 
			outraster <- writeRaster(outRaster, filetype=filetype, overwrite=overwrite) 
		}
	} else {
		if (dataContent(x) == 'nodata'  &  dataSource(x) == 'ram' ) {
			stop('values for x are not available')
		}
		if (dataContent(y) == 'nodata'  &  dataSource(y) == 'ram' ) {
			stop('values for y are not available')
		}
		
		if (!canProcessInMemory(x, 4) && filename == '') {
			filename <- tempfile()
			outraster <- setFilename(outraster, filename )
			if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
		}
		starttime <- proc.time()
		
		v <- vector(length=0)
		for (r in 1:nrow(outRaster)) {
			x <- readRow(x, r)
			y <- readRow(y, r)
			vals <- values(x)
			vals[is.na(vals)] <- values(y)[is.na(vals)] 
			if (filename == "") {
				v <- c(v, vals)
			} else {
				outRaster <- setValues(outRaster, vals, r)
				outRaster <- writeRaster(outRaster, filetype=filetype, overwrite=overwrite)
			}
			
			if (r %in% track) { .showTrack(r, track, starttime) }
			
		}
		if (filename(outRaster) == "") {
			outRaster <- setValues(outRaster, v)
		}
	}
	return(outRaster)
}
)
