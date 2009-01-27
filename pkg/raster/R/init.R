# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3


init <- function(raster, fun=runif, filename="", overwrite=FALSE, asInt=FALSE) {
	filename <- trim(filename)
	outraster <- setRaster(raster, filename)
	if (asInt) {setDatatype(outraster, 'integer') }

	if ( dataContent(raster) == 'all' | dataSource(raster) == 'ram' ) {
		n <- ncell(raster)
		outraster <- setValues(outraster, fun(n)) 
		if (filename != "") {	
			outraster <- writeRaster(outraster, overwrite=overwrite) 
		}
		
	} else if (dataSource(raster) == 'disk') {
		n <- length(ncol(raster))
		v <- vector(length=0)

		for (r in 1:nrow(raster)) {
			if (filename(outraster) == "") {
				v <- c(v, fun(n))
			} else {			
				outraster <- setValues(outraster, fun(n), r) 
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}	
		}	
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}


isNA <- function(raster, value=0, filename="", overwrite=FALSE, asInt=FALSE) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	raster <- calc(raster, fun, filename, overwrite=overwrite, asInt=asInt)
	return(raster) 
}

	
setNA <- function(raster, operator= "<=", value=0, filename="", overwrite=FALSE, asInt=FALSE) {
	if (operator == ">") { fun <- function(x) { x[x>value] <- NA; return(x)}
	} else if (operator == "<") { fun <- function(x) { x[x<value] <- NA; return(x)}
	} else if (operator == "<=") { fun <- function(x) { x[x<=value] <- NA; return(x)}
	} else if (operator == ">=") { fun <- function(x) { x[x>=value] <- NA; return(x)}
	} else if (operator == "==") { fun <- function(x) { x[x==value] <- NA; return(x)}
	} else if (operator == "!=") { fun <- function(x) { x[x!=value] <- NA; return(x)}
	}
	return(calc(raster, fun, filename, overwrite=overwrite, asInt=asInt))
}


