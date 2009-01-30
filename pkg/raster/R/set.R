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

setRes <- function(object, xres, yres=xres) {
	if (extends(class(object), "Raster")) {
		object <- clearValues(object)
	}
	bb <- getBbox(object)
	nc <- round( (bb@xmax - bb@xmin) / xres )
	nr <- round( (bb@ymax - bb@ymin) / yres )
	bb@xmax <- bb@xmin + nc * xres
	bb@ymin <- bb@ymax - nr * yres
	object	<- setBbox(object, bb)
	object <- setRowCol(object, nr, nc)
	return(object)
}

setRaster <- function(object, filename="", values=NA) {

	if (class(object) == 'RasterStack') { object <- asRasterLayer(object, 1) }
	if (class(object) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }

	filename <- trim(filename)
	if (filename != "" & filename == filename(object)) {
		stop("it is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
	}

	raster <- raster(xmn = xmin(object), xmx = xmax(object), ymn = ymin(object), ymx = ymax(object), nrows=nrow(object), ncols=ncol(object), projstring=projection(object))
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
		if (nbands(object) > 1) { shortname <- paste(shortname, "_", band(object)) } 
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
			raster@data@min <- min(vals)
			raster@data@max <- max(vals)
		} else {
			raster@data@min <- NA
			raster@data@max <- NA
		}
	}
#	if (raster@file@datatype == 'logical') {
#		raster@data@min <- as.logical(raster@data@min)
#		raster@data@max <- as.logical(raster@data@max)
#	}
	raster@data@haveminmax <- TRUE
	return(raster)
}

