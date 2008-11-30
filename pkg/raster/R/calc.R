# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3



calc <- function(raster, fun=sqrt, filename="", overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}
	
	if (!(dataContent(raster) == 'all' | dataContent(raster) == 'sparse' | dataSource(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
	if ( dataContent(raster) == 'all') {
		outraster <- set.values(outraster, fun(values(raster))) 
		if (filename(outraster)!="") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if ( dataContent(raster) == 'sparse') {
		outraster <- set.values.sparse(outraster, fun(values(raster)),  dataIndices(raster)) 
		if (filename(outraster) != "") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if (dataSource(raster) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(raster)))
			} else {
				outraster <- set.values.row(outraster, fun(values(raster)), r)
				outraster <- write.row(outraster, overwrite=overwrite)
			}
		}
		if (filename(outraster) == "") { outraster <- set.values(outraster, v) }
	}
	return(outraster)
}



