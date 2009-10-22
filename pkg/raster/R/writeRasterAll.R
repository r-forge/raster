# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeRasterAll <- function(raster, filetype, ... ) {

	raster@file@driver <- filetype

 	filename <- .writefilename(raster, ...)
	filename <- .setFileExtensionHeader(filename, filetype)
	.setFilename(raster) <- filename
	fnamevals <- .setFileExtensionValues(filename)

	
	overwrite <- .overwrite(...)
	if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
		stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
#	raster@file@driver <- 'raster'
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- setMinMax(raster)

	datatype <- .datatype(...)
	
	dtype <- .shortDataType(datatype)
	
	if ( datatype =='INT') {
	# optimize the number of bytes within the datatype
		intround <- TRUE
		if (xmin(raster) > -128 & xmax(raster) < 128) {
			.setDataType(raster) <- 'INT1S'
		} else if (xmin(raster) >=0 & xmax(raster) < 256) {
			.setDataType(raster) <- 'INT1U'
		} else if (xmin(raster) > -32767 & xmax(raster) < 32768) {
			.setDataType(raster) <- 'INT2S'
		} else if (xmin(raster) >= 0 & xmax(raster) < 65534 ) {
			.setDataType(raster) <- 'INT2U'
		} else if (xmin(raster) > -2147483647 & xmax(raster) < 2147483648 ) {
			.setDataType(raster) <- 'INT4S'
		} else if (xmin(raster) >= 0 & xmax(raster) < 4294967294 ) {
			.setDataType(raster) <- 'INT4U'
		} else if (xmin(raster) > -(2^63/2) & xmax(raster) < (2^64/2)) {
			.setDataType(raster) <- 'INT8S'
		} else if (xmin(raster) >= 0 & xmax(raster) < 2^64) {
			.setDataType(raster) <- 'INT8U'
		} else {
			intround <- FALSE
			.setDataType(raster) <- 'FLT8S'
			raster@data@values <- as.numeric(values(raster))
		}
		if (intround) {
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)				
		}
	} else if ( datatype =='FLT') {
		if (xmin(raster) < -3.4E38 | xmax(raster) > 3.4E38) {
			.setDataType(raster) <- 'FLT8S'
		} else {
			.setDataType(raster) <- 'FLT4S'
		}	
	} else if ( dtype =='LOG') {
		raster@data@values <- as.integer(values(raster))
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	}

	attr(raster@file, "con") <- file(fnamevals, "wb")
	if (raster@data@content == 'sparse') { 
		raster <- .writeSparse(raster, overwrite=overwrite) 
	} else {
		dsize <- dataSize(raster@file@datanotation)
		writeBin( values(raster), raster@file@con, size = dsize ) 
		writeRasterHdr(raster, filetype) 
	}
	close(raster@file@con)
#attr(raster@file, "con") <- file(fnamevals, "rb")
# put logical values back to T/F
	if ( dtype =='LOG' ) {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
		raster@data@values <- as.logical(values(raster))
	} else if ( dtype =='INT' ) {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
	}
	return(raster)
}
 
 