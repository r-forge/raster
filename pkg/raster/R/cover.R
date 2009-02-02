# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


cover <- function(x, y, filename="", overwrite=TRUE) {
	if (class(x) != 'RasterLayer' | class(y) != 'RasterLayer') {
		stop('first two arguments should be objects of class "RasterLayer"')
	}
	if (!compare(c(x, y))) { 
		stop('rasters do not match') 
	}
	outRaster <- setRaster(x, filename)
	if ( dataContent(x) == 'all' & dataContent(y) == 'all') {
		x@data@values[is.na(x@data@values)] <- values(y)[is.na(x@data@values)]
		rm(y)
		outRaster <- setValues(outRaster, values(x))
		if (filename(outRaster) != "") { 
			writeRaster(outRaster, overwrite=overwrite) 
		}
	} else {
		if (dataContent(x) == 'nodata'  &  dataSource(x) == 'ram' ) {
			stop('values for x are not available')
		}
		if (dataContent(y) == 'nodata'  &  dataSource(y) == 'ram' ) {
			stop('values for y are not available')
		}
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
				outRaster <- writeRaster(outRaster, overwrite=overwrite)
			}
		}
		if (filename(outRaster) == "") {
			outRaster <- setValues(outRaster, v)
		}
	}
	return(outRaster)
}

