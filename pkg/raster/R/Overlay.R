# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3



setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, fun=function(x,y){return(x+y)}, filename="", overwrite=FALSE, ...){ 
		return(.Overlay(x, y, fun, filename, overwrite) )
	}
)


.Overlay <- function(raster1, raster2, fun=function(x,y){return(x+y)}, filename="", overwrite=FALSE) {
	if (class(raster1) != 'RasterLayer' | class(raster2) != 'RasterLayer') {
		stop('first two arguments should be objects of class "RasterLayer"')
	}
	if (!compare(c(raster1, raster2))) { 
		stop('Extent and/or resolution of rasters do not match') 
	}
	outraster <- setRaster(raster1)
	outraster <- setFilename(outraster, filename)

	if ( dataContent(raster1) == 'all' &  dataContent(raster2) == 'all') {
		vals <- fun( values(raster1), values(raster2) )
		outraster <- setValues(outraster, vals)
		if (filename(outraster) != "") { writeRaster(outraster, overwrite=overwrite) }
		
	} else if ( dataSource(raster1) == 'disk' &  dataSource(raster2) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(outraster)) {
			raster1 <- readRow(raster1, r)
			raster2 <- readRow(raster2, r)
			vals <- fun(values(raster1), values(raster2))
			if (filename(outraster) == "") {
				v <- c(v, vals)
			} else {
				outraster <- setValues(outraster, vals, r)
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}	
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} else {
		stop('values of rasters must be either all in memory or all on disk')
	}
	return(outraster)
}

