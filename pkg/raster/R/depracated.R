
#newRaster <- function(...xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
#	stop("'newRaster' is deprecated. Use 'raster' instead")
#}


rasterFromFile <- function(filename, values=FALSE, band=1) {
	stop("'rasterFromFile' is deprecated. Use 'raster(filename)' instead")
}	


...isNA <- function(raster, value=0, filename="", overwrite=FALSE, asInt=FALSE) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	if (asInt) { datatype <- 'INT4S' } else { datatype <- 'FLT4S' }
	raster <- calc(raster, fun, filename, overwrite=overwrite, datatype )
	return(raster) 
}

	
...setNA <- function(raster, operator= "<=", value=0, filename="", overwrite=FALSE, asInt=FALSE) {
	if (operator == ">") { fun <- function(x) { x[x>value] <- NA; return(x)}
	} else if (operator == "<") { fun <- function(x) { x[x<value] <- NA; return(x)}
	} else if (operator == "<=") { fun <- function(x) { x[x<=value] <- NA; return(x)}
	} else if (operator == ">=") { fun <- function(x) { x[x>=value] <- NA; return(x)}
	} else if (operator == "==") { fun <- function(x) { x[x==value] <- NA; return(x)}
	} else if (operator == "!=") { fun <- function(x) { x[x!=value] <- NA; return(x)}
	}
	if (asInt) { datatype <- 'INT4S' } else { datatype <- 'FLT4S' }
	return( calc(raster, fun, filename, overwrite=overwrite, datatype))
}

