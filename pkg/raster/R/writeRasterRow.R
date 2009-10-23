# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.startRowWriting <- function(raster, filename, doPB=FALSE, ...) {
 	filename <- trim(filename)
	if (filename == "") {
		stop('RasterLayer has no filename; and no filename specified as argument to writeRaster')
	}
	filetype <- .filetype(...)
	filename <- .setFileExtensionHeader(filename, filetype)
	.setFilename(raster) <- filename
	fnamevals <- .setFileExtensionValues(filename)
	datatype <- .datatype(...)
	.setDataType(raster) <- datatype
	
	if (filetype %in% c('SAGA')) {
		resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
		if (resdif > 0.01) {
			stop(paste("raster has unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"))
		}
	}

	overwrite <- .overwrite( ...)
	if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
		stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
	attr(raster@file, "con") <- file(fnamevals, "wb")
	attr(raster@file, "dsize") <- dataSize(raster@file@datanotation)
	attr(raster@file, "dtype") <- .shortDataType(raster@file@datanotation)
	
	raster@data@min <- Inf
	raster@data@max <- -Inf
	raster@data@haveminmax <- FALSE
	raster@file@driver <- filetype
	
	if (doPB)  { attr(raster@file, "pb") <- pbCreate(nrow(raster), type=.progress() ) }
	
	return(raster)
}


.stopRowWriting <- function(raster, doPB=FALSE) {
	writeRasterHdr(raster, .driver(raster)) 
	close(raster@file@con)
	fnamevals <- .setFileExtensionValues(raster@file@name)
#	attr(raster@file, "con") <- file(fnamevals, "rb")
	raster@data@haveminmax <- TRUE
	if (raster@file@dtype == "INT") {
		raster@data@min <- round(raster@data@min)
		raster@data@max <- round(raster@data@max)
	} else if ( raster@file@dtype =='LOG' ) { 
#		raster@data@min <- as.logical(raster@data@min)
#		raster@data@max <- as.logical(raster@data@max)
	}
	raster@data@source <- 'disk'
	raster@data@content <- 'nodata'
	raster@file@driver <- 'raster'
	raster@data@values <- vector(length=0)

	if (doPB) {
		pbClose( attr(raster@file, "pb") )
		attr(raster@file, "pb") <- ''
	}

	return(raster)
}		
 
 
.writeRasterRow <- function(raster, filename, doPB=FALSE, ...) {

	if (dataIndices(raster)[1] == 1) { 
		raster <- .startRowWriting(raster, filename, ...)
 	} 

	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	if (raster@file@dtype == "INT" || raster@file@dtype =='LOG' ) { 
		values <- as.integer(round(raster@data@values))  
		values[is.na(values)] <- as.integer(raster@file@nodatavalue)		
	} else { 
		values  <- as.numeric( raster@data@values ) 
	}
	
	rsd <- na.omit(raster@data@values) # min and max values
	if (length(rsd) > 0) {
		raster@data@min <- min(raster@data@min, min(rsd))
		raster@data@max <- max(raster@data@max, max(rsd))
	}	
	
	writeBin(values, raster@file@con, size = raster@file@dsize )
	
	if (dataIndices(raster)[2] >= ncell(raster)) {
		raster <- .stopRowWriting(raster)
		if (dataIndices(raster)[2] > ncell(raster)) {
			warning(paste('You have written beyond the end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
		}
	}
	if (doPB) {
		pbStep( attr(raster@file, "pb"), row )
	}

	return(raster)	
}

