# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeRasterAll <- function(raster, filename, ... ) {

	filetype <- .filetype(...)
	raster@file@driver <- filetype
 	filename <- trim(filename)
	filename <- .setFileExtensionHeader(filename, filetype)
	raster@file@name <- filename
	fnamevals <- .setFileExtensionValues(filename, filetype)
	
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
	
	mn <- minValue(raster)
	mx <- maxValue(raster)
	if ( dtype =='INT') {
		intround <- TRUE
		if (datatype == 'INT') {
		# optimize the number of bytes within the datatype
			if (mn > -128 & mx < 128) {
				dataType(raster) <- 'INT1S'
			} else if (mn >=0 & mx < 256) {
				dataType(raster) <- 'INT1U'
			} else if (mn > -32767 & mx < 32768) {
				dataType(raster) <- 'INT2S'
			} else if (mn >= 0 & mx < 65534 ) {
				dataType(raster) <- 'INT2U'
			} else if (mn > -2147483647 & mx < 2147483648 ) {
				dataType(raster) <- 'INT4S'
			} else if (mn >= 0 & mx < 4294967294 ) {
				dataType(raster) <- 'INT4U'
			} else if (mn > -(2^63/2) & mx < (2^64/2)) {
				dataType(raster) <- 'INT8S'
			} else if (mn >= 0 & mx < 2^64) {
				dataType(raster) <- 'INT8U'
			} else {
				intround <- FALSE
				dataType(raster) <- 'FLT8S'
				raster@data@values <- as.numeric(raster@data@values )
			}
		}
		if (intround) {
			raster@data@values <- as.integer(round(raster@data@values ))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)				
		}
	} else if ( dtype =='FLT') {
		raster@data@values <- as.numeric(raster@data@values)
		if (datatype == 'FLT') {
			if (mn < -3.4E38 | mx > 3.4E38) {
				dataType(raster) <- 'FLT8S'
			} else {
				dataType(raster) <- 'FLT4S'
			}
		}	
	} else if ( dtype =='LOG') {
		raster@data@values <- as.integer(raster@data@values)
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	}

	attr(raster@file, "con") <- file(fnamevals, "wb")
#	if (raster@data@content == 'sparse') { 
#		raster <- .writeSparse(raster, filename=filename, overwrite=overwrite) 
#	} else {
	dsize <- dataSize(raster@file@datanotation)
	writeBin(raster@data@values , raster@file@con, size = dsize ) 
	writeRasterHdr(raster, filetype) 

	close(raster@file@con)
#attr(raster@file, "con") <- file(fnamevals, "rb")
# put logical values back to T/F
	if ( dtype =='LOG' ) {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
		raster@data@values <- as.logical(values(raster))
	} else if ( dtype =='INT' ) {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
	}
	
	raster@data@source <- 'disk'
	raster@file@driver <- 'raster'
	
	return(raster)
}
 
 