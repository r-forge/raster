# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

.addHistory <- function(raster, message) {
	if (is.character(message) & message != "") {
		raster@history <- c(message, raster@history)
	}	
}

setRowCol <- function(raster, nrows=nrow(raster), ncols=ncol(raster)) {
	raster <- clearValues(raster)
	raster@ncols <- as.integer(ncols)
	raster@nrows <- as.integer(nrows)
	return(raster)
}

setRaster <- function(object, filename="", values=NA) {
	if (class(object) == 'RasterStack') { object <- asRasterLayer(object, 1) }
	if (class(object) == 'RasterBrick') { object <- asRasterLayer(object, 1) }
	if (class(object) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }

	filename <- trim(filename)
	if (filename != "" & filename == filename(object)) {
		stop("it is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
	}

	raster <- newRaster(xmn = xmin(object), xmx = xmax(object), ymn = ymin(object), ymx = ymax(object), nrows=nrow(object), ncols=ncol(object), projstring=projection(object))
	raster <- setFilename(raster, filename)
	
	if ( length(values) != 1 | ( length(values) == 1 & ncell(raster) == 1) ) {
		raster <- setValues(raster, values)
	}
	return(raster)
}

setFilename <- function(object, filename) {
	if (is.na(filename)) {filename <- ""}
	filename <- trim(filename)
	if (class(object)=='RasterStack') {
		object@filename <- setFileExtension(filename, ".stk")
	} else {
		object@file@name <- filename
	}	
	if (class(object)=='RasterLayer') {
		shortname <- fileName(filename)
		shortname <- setFileExtension(shortname, "")
		shortname <- gsub(" ", "_", shortname)
		if (object@file@nbands > 1) { shortname <- paste(shortname, "_", object@file@band) } 
		object@file@shortname <- shortname
		object@file@gdalhandle <- list()
	}	
	return(object)	
}


setProjection <- function(object, projstring) {
	if (class(projstring)=="CRS") {
		object@crs <- projstring
	} else {	
		object@crs <- newCRS(projstring)
	}	
	return(object)
}


newCRS <- function(projstring) {
	projstring <- trim(projstring)
	if (is.na(projstring) | nchar(projstring) < 3) { 
		projs <- (CRS(as.character(NA)))
	} else {
		projs <- try(CRS(projstring), silent = T)
		if (class(projs) == "try-error") { 
			warning(paste(projstring, 'is not a valid proj4 CRS string')) 
			projs <- CRS(as.character(NA))
		}
	}
	return(projs)
}



roundCoords <- function(object, digits=0) {
	digits <- max(0, digits)
	b <- getBbox(object)
	b@xmin <- round(b@xmin, digits)
	b@xmax <- round(b@xmax, digits)
	b@ymin <- round(b@ymin, digits)
	b@ymax <- round(b@ymax, digits)
	if (class(object) == 'BoundingBox') {
		return(b)
	}
	object <- setBbox(object, b)
	return(object)
}


	
setMinMax <- function(raster, readfromdisk=FALSE) {
	if (dataContent(raster) != 'all' & dataContent(raster) != 'sparse') {
		if (readfromdisk) {
			raster@data@min <- 3e34
			raster@data@max <- -3e34
			for (r in 1:nrow(raster)) {
				raster <- readRow(raster, r)
				rsd <- na.omit(values(raster)) # min and max values
				if (length(rsd) > 0) {
					raster@data@min <- min(minValue(raster), min(rsd))
					raster@data@max <- max(maxValue(raster), max(rsd))
				}	
			}
			raster <- clearValues(raster)
		} else {
			stop('no data in memory, and readfromdisk=FALSE')
		}	
	} else {
		vals <- na.omit(values(raster)) # min and max values
		if (length(vals) > 0) {
			raster@data@min <-  min(vals)
			raster@data@max <- max(vals)
		} else {
			raster@data@min <- NA
			raster@data@max <- NA
		}
	}	
	raster@data@haveminmax <- TRUE
	return(raster)
}


setDatatype <- function(raster, datatype, datasize=4) {
#  signed"  should become variable
	signed <- TRUE 
	if (datatype == "numeric") {
		raster@file@datatype <- datatype 
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.numeric(values(raster))
		}
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -3.4E38
			raster@file@datanotation <- "FLT4S"
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <-  -1.7E308
			raster@file@datanotation <- "FLT8S"
		} else { 
			stop("invalid datasize for this datatype") 
		}
	} else if (datatype == "integer") {
		raster@file@datatype <- datatype 
		raster@data@min <- round(minValue(raster))
		raster@data@max <- round(maxValue(raster))
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.integer(round(values(raster)))
		}
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -2147483647
			raster@file@datanotation <- "INT4S"
		} else if (datasize == 2) {
			raster@file@datasize <- as.integer(2)
			raster@file@nodatavalue <- -32768
			raster@file@datanotation <- "INT2S"
		} else if (datasize == 1) {
			raster@file@datasize <- as.integer(1)
			raster@file@nodatavalue <- -1
			raster@file@datanotation <- "INT1U"
			warning("binary files of single byte do not have NA values on disk")
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <- -2^63
			raster@file@datanotation <- "INT8S"
		} else {
			stop("invalid datasize for this datatype") 
		}
#	} else if ( datatype == 'logical' ) {
#		raster@file@datatype <- datatype 
#		raster@data@min <- round(minValue(raster))
#		raster@data@max <- round(maxValue(raster))
#		raster@file@datasize <- as.integer(2)
#		raster@file@nodatavalue <- -32768
#		raster@file@datanotation <- "LOGICAL"
	} else {
		stop("unknown datatype")
	} 
	return(raster)
}

