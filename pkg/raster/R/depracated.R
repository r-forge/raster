
#newRaster <- function(...xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
#	stop("'newRaster' is deprecated. Use 'raster' instead")
#}


rasterFromFile <- function(filename, values=FALSE, band=1) {
	stop("'rasterFromFile' has been removed. Use 'raster(filename)' instead")
}	


..shortFileName <- function(filename) {
# is this the same as basename ?
    filename <- gsub("\\\\", "/", filename)
	if (filename == "") {return(filename)
	} else {
		split <- strsplit(filename, "/")
		l <- length(split[[1]])
		shortfilename <- split[[1]][[l]]
		return(shortfilename)
	}	
}   
   
..path <- function(filename) {
#  use dirname instead
    filename <- gsub("\\\\", "/", filename)
	file <- ..shortFileName(filename)
	path <- gsub(file, '', filename)
	return(path)
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

