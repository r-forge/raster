# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.setFileExtensionValues <- function(fname) {
	fname <- setFileExtension(fname, ".gri")
	return(fname)
}
 
.setFileExtensionHeader <- function(fname) {
	fname <- setFileExtension(fname, ".grd")
	return(fname)
}
 

.writeRasterAll <- function(raster, overwrite=FALSE) {

	fname <- trim(raster@file@name)
	if (fname == "") {
		stop('first provide a filename. E.g.: raster <- setFilename(raster, "c:/myfile")')
	}
	fname <- .setFileExtensionHeader(fname)
	filename(raster) <- fname
	fnamevals <- .setFileExtensionValues(fname)

	if (!overwrite & (file.exists(fname) | file.exists(fnamevals))) {
		stop(paste(fname,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
#	raster@file@driver <- 'raster'
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- setMinMax(raster)

	dtype <- .shortDataType(raster@file@datanotation)
	
	if ( dtype =='INT') {
		if (xmin(raster) > -32767 & xmax(raster) < 32768) {
			raster <- setDatatype(raster, 'INT2S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)						
		} else if (xmin(raster) > -2147483647 & xmax(raster) < 2147483648 ) {
			raster <- setDatatype(raster, 'INT4S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)			
		} else if (xmin(raster) > -(2^63/2) & xmax(raster) < (2^64/2)) {
			raster <- setDatatype(raster, 'INT8S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)			
		} else {
			raster <- setDatatype(raster, 'FLT8S')
			raster@data@values <- as.numeric(values(raster))
		}
	} else if ( dtype =='LOGICAL') {
		raster@data@values <- as.integer(values(raster))
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	} else {
		if (xmin(raster) < -3.4E38 | xmax(raster) > 3.4E38) {
			raster <- setDatatype(raster, 'FLT8S')
		} else {
			raster <- setDatatype(raster, 'FLT4S')
		}	
	}

	attr(raster@file, "con") <- file(fnamevals, "wb")
	if (raster@data@content == 'sparse') { 
		raster <- .writeSparse(raster, overwrite=overwrite) 
	} else {
		dsize <- dataSize(raster@file@datanotation)
		writeBin( values(raster), raster@file@con, size = dsize ) 
		.writeRasterHdr(raster) 
	}
	close(raster@file@con)
	attr(raster@file, "con") <- file(fnamevals, "rb")
	
	# put logical values back to T/F
	if ( dtype =='logical') {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
		raster@data@values <- as.logical(values(raster))
	}

	return(raster)
}
 
 



