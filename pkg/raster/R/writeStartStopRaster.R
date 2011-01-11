# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.startRasterWriting <- function(raster, filename, NAflag, bandorder='BIL', update=FALSE, ...) {
 	filename <- trim(filename)
	if (filename == "") {
		stop('missing filename')
	}
	filetype <- .filetype(...)
		
	filename <- .setFileExtensionHeader(filename, filetype)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	datatype <- .datatype(...)
	
	dataType(raster) <- datatype
	if (!missing(NAflag)) {
		raster@file@nodatavalue <- NAflag
	}
	
	if (filetype %in% c('SAGA')) {
		resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
		if (resdif > 0.01) {
			stop(paste("raster has unequal horizontal and vertical resolutions. Such data cannot be stored in SAGA format"))
		}
	}

	overwrite <- .overwrite( ...)
	if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
		stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
	if (update) {
		attr(raster@file, "con") <- file(fnamevals, "r+b")
	} else {
		attr(raster@file, "con") <- file(fnamevals, "wb")
	}
	attr(raster@file, "dsize") <- dataSize(raster@file@datanotation)
	attr(raster@file, "dtype") <- .shortDataType(raster@file@datanotation)
	
	raster@data@min <- rep(Inf, nlayers(raster))
	raster@data@max <- rep(-Inf, nlayers(raster))
	raster@data@haveminmax <- FALSE
	raster@file@driver <- filetype
	raster@file@name <- filename
	if (! bandorder %in% c('BIL', 'BIP', 'BSQ')) {
		stop('bandorder must be one of "BIL", "BSQ", or "BIP"')
	}
	raster@file@bandorder <- bandorder
	
	return(raster)
}


.stopRasterWriting <- function(raster) {
	close(raster@file@con)
#	fnamevals <- .setFileExtensionValues(raster@file@name)
#	attr(raster@file, "con") <- file(fnamevals, "rb")
	raster@data@haveminmax <- TRUE
	if (raster@file@dtype == "INT") {
		raster@data@min <- round(raster@data@min)
		raster@data@max <- round(raster@data@max)
	} else if ( raster@file@dtype =='LOG' ) { 
#		raster@data@min <- as.logical(raster@data@min)
#		raster@data@max <- as.logical(raster@data@max)
	}
	hdr(raster, .driver(raster)) 
	filename <- .setFileExtensionValues(filename(raster), raster@file@driver)
	
	if (inherits(raster, 'RasterBrick')) {
		r <- brick(filename, native=TRUE)
	} else {
		r <- raster(filename, native=TRUE)
	}
	
	if (! r@data@haveminmax) {
		r@data@min <- raster@data@min
		r@data@max <- raster@data@max
		r@data@haveminmax <- TRUE
	}
	return(r)
}		
 
 