# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

setDatatype <- function(raster, datatype, datasize=4, signed=TRUE) {
	if (datatype == "numeric") {
		raster@file@datasigned <- TRUE
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
		raster@file@datasigned <- signed
		raster@file@datatype <- datatype 
		raster@data@min <- round(minValue(raster))
		raster@data@max <- round(maxValue(raster))
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.integer(round(values(raster)))
		}
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			if (signed) {
				raster@file@datanotation <- "INT4S"
				raster@file@nodatavalue <- -2147483647
			} else {
				raster@file@datanotation <- "INT4U"
				raster@file@nodatavalue <- 4294967295
			}
		} else if (datasize == 2) {
			raster@file@datasize <- as.integer(2)
			if (signed) {
				raster@file@datanotation <- "INT2S"
				raster@file@nodatavalue <- -32768
			} else {
				raster@file@datanotation <- "INT2U"
				raster@file@nodatavalue <- 65535
			}
		} else if (datasize == 1) {
			raster@file@datasize <- as.integer(1)
			# there is no nodata value for byte
			raster@file@nodatavalue <- -9999
			if (signed) {
				raster@file@datanotation <- "INT1S"			
			} else {
				raster@file@datanotation <- "INT1U"
			}
			warning("binary files of a single byte do not have NA values on disk")
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			if (signed) {
				raster@file@datanotation <- "INT8S"
				raster@file@nodatavalue <- -9223372036854775808
			} else {
				raster@file@datanotation <- "INT8U"	
				raster@file@nodatavalue <- 18446744073709551615
			}
		} else {
			stop("invalid datasize for this datatype") 
		}
	} else if ( datatype == 'logical' ) {
		raster@file@datasigned <- TRUE
		raster@file@datatype <- datatype 
		raster@file@datasize <- as.integer(1)
		raster@file@nodatavalue <- -127
		raster@file@datanotation <- "LOGICAL"
	} else {
		stop("unknown datatype")
	} 
	return(raster)
}

