# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3


init <- function(raster, fun=runif, filename="", overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}

	if ( dataContent(raster) == 'all' | dataSource(raster) == 'ram' ) {
		n <- ncells(raster)
		outraster <- set.values(outraster, fun(n)) 
		if (!is.na(filename)) {	outraster <- write.raster(outraster, overwrite=overwrite) }
		
	} else if (dataSource(raster) == 'disk') {
		n <- length(ncol(raster))
		v <- vector(length=0)

		for (r in 1:nrow(raster)) {
			if (filename(outraster)=="") {
				v <- c(v, fun(n))
			} else {			
				outraster <- set.values.row(outraster, fun(n), r) 
				outraster <- write.row(outraster, overwrite=overwrite)
			}	
		}	
		if (filename(outraster) == '') { outraster <- set.values(outraster, v) }
	} 
	return(outraster)
}


isNA <- function(raster, value=0, filename="", overwrite=FALSE, INT=FALSE) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	raster <- calc(raster, fun, filename, overwrite=overwrite, INT=INT)
	return(raster) 
}

	
setNA <- function(raster, operator= "<=", value=0, filename="", overwrite=FALSE, INT=FALSE) {
	if (operator == ">") { fun <- function(x) { x[x>value] <- NA; return(x)}
	} else if (operator == "<") { fun <- function(x) { x[x<value] <- NA; return(x)}
	} else if (operator == "<=") { fun <- function(x) { x[x<=value] <- NA; return(x)}
	} else if (operator == ">=") { fun <- function(x) { x[x>=value] <- NA; return(x)}
	} else if (operator == "==") { fun <- function(x) { x[x==value] <- NA; return(x)}
	} else if (operator == "!=") { fun <- function(x) { x[x!=value] <- NA; return(x)}
	}
	return(calc(raster, fun, filename, overwrite=overwrite, INT=INT))
}


