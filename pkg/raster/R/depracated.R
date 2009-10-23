

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


...isNA <- function(raster, value=0, filename="", asInt=FALSE, ...) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	if (asInt) { datatype <- 'INT4S' } else { datatype <- 'FLT4S' }
	raster <- calc(raster, fun, filename=filename, datatype=datatype, ... )
	return(raster) 
}

	
...setNA <- function(raster, operator= "<=", value=0, filename="", asInt=FALSE, ...) {
	if (operator == ">") { fun <- function(x) { x[x>value] <- NA; return(x)}
	} else if (operator == "<") { fun <- function(x) { x[x<value] <- NA; return(x)}
	} else if (operator == "<=") { fun <- function(x) { x[x<=value] <- NA; return(x)}
	} else if (operator == ">=") { fun <- function(x) { x[x>=value] <- NA; return(x)}
	} else if (operator == "==") { fun <- function(x) { x[x==value] <- NA; return(x)}
	} else if (operator == "!=") { fun <- function(x) { x[x!=value] <- NA; return(x)}
	}
	if (asInt) { datatype <- 'INT4S' } else { datatype <- 'FLT4S' }
	return( calc(raster, fun, filename=filename, datatype=datatype, ...))
}

