# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3


init <- function(raster, fun=runif, filename="", overwrite=FALSE, datatype = 'FLT4S', filetype='raster', track=-1) {

	outraster <- setRaster(raster, filename)
	outraster <- setDatatype(outraster, datatype)
	
	if ( dataContent(raster) == 'all' | dataSource(raster) == 'ram' ) {
		n <- ncell(raster)
		outraster <- setValues(outraster, fun(n)) 
		if (filename(outraster) != "") {	
			outraster <- writeRaster(outraster, overwrite=overwrite) 
		}
		
	} else if (dataSource(raster) == 'disk') {
		starttime <- proc.time()
		n <- length(ncol(raster))
		v <- vector(length=0)

		for (r in 1:nrow(raster)) {
			if (filename(outraster) == "") {
				v <- c(v, fun(n))
			} else {			
				outraster <- setValues(outraster, fun(n), r) 
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}	
			if (r %in% track) {
				elapsed <- (proc.time() - starttime)[3]
				tpr <- elapsed /r
				ttg <- round(tpr/60 * (nrow(raster) - r), digits=1)
				cat('row', r, '-', ttg, 'minutes to go\n')
			}
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}


