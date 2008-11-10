# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


r.overlay <- function(raster1, raster2, fun=function(x,y){return(x+y)}, filename="", overwrite=TRUE) {
	if (class(raster1) != 'RasterLayer' | class(raster2) != 'RasterLayer') {
		stop('first two arguments should be objects of class "RasterLayer"')
	}
	if (!compare(c(raster1, raster2))) { 
		stop() 
	}
	outraster <- set.raster(raster1)
	outraster <- set.filename(outraster, filename)
	
	if ( data.content(raster1) == 'all' &  data.content(raster2) == 'all') {
		vals <- fun( values(raster1), values(raster2) )
		outraster <- set.values(outraster, vals)
		if (filename != "") { write.raster(outraster, overwrite=overwrite) }
		
	} else if ( data.source(raster1) == 'disk' &  data.source(raster2) == 'disk') {
		for (r in 1:nrow(outraster)) {
			raster1 <- read.row(raster1, r)
			raster2 <- read.row(raster2, r)
			vals <- fun(values(raster1), values(raster2))
			outraster <- set.values.row(outraster, vals, r)
			outraster <- write.row(outraster, overwrite=overwrite)
		}
	} else {
		stop('data must be either in memory or on disk')
	}
	return(outraster)
}


r.cover <- function(raster1, raster2, filename="", overwrite=TRUE) {
	if (class(raster1) != 'RasterLayer' | class(raster2) != 'RasterLayer') {
		stop('first two arguments should be objects of class "RasterLayer"')
	}
	if (!compare(c(raster1, raster2))) { 
		stop() 
	}
	outraster <- set.raster(raster1, filename)
	if ( data.content(raster1) == 'all' &  data.content(raster2) == 'all') {
		vals <- values(raster1)
		vals[is.na(vals)] <- values(raster2)[is.na(vals)]
		outraster <- set.values(outraster, vals)
		if (filename != "") { write.raster(outraster, overwrite=overwrite) }
	} else if ( data.source(raster1) == 'disk' &  data.source(raster2) == 'disk') {
		for (r in 1:nrow(outraster)) {
			raster1 <- read.row(raster1, r)
			raster2 <- read.row(raster2, r)
			vals <- values(raster1)
			vals[is.na(vals)] <- values(raster2) 
			outraster <- set.values.row(outraster, vals, r)
			outraster <- write.row(outraster, overwrite=overwrite)
		}
	} else {
		stop('data must be either in memory or on disk')
	}
	return(outraster)
}


