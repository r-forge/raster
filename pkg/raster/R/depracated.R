
#newRaster <- function(...xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
#	stop("'newRaster' is deprecated. Use 'raster' instead")
#}


setDatatype <- function(x, value) {
	warning('depracated function. Use "dataType(x) <- value"')
	dataType(x) <- value
	return(x)
}


setFilename <- function(x, value) {
	warning('depracated function. Use "filename(x) <- value"')
	filename(x) <- value
	return(x)
}


setFileExtension <- function(x, value) {
	stop('depracated function. Use "fileExtension(x) <- value"')
}

setRaster <- function(object, filename="", values=NULL) {
	warning('depracated, use "raster()" instead')
	return(raster(x=object, filename=filename, values=values))
}

rasterFromBbox <- function(bndbox, nrows=10, ncols=10) {
	warning("'rasterFromBbox' is deprecated. Use 'raster(bbox, ...)' instead")
	return(raster(x=bndbox, nrows=nrows, ncols=ncols))
}

rasterFromFile <- function(filename, values=FALSE, band=1) {
	warning("'rasterFromFile' is deprecated. Use 'raster(filename)' instead")
	return(raster(x=filename, values=values, band=band))
}	


# no longer used. Use calc instead. See ?calc


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

