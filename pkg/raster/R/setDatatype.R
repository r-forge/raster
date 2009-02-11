# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

setDatatype <- function(raster, datatype) {
	datatype <- trim(datatype)
	if (!(datatype %in% c('LOGICAL', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'INT4U', 'INT8U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- substr(datatype,1,3)
	size <- substr(datatype,4,4)
	signed <- substr(datatype,1,3) != 'U'
	
	raster@file@datanotation <- datatype
	
	if (type == "FLT") {
		raster@file@datatype <- 'numeric'
		raster@file@datasigned <- TRUE
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.numeric(values(raster))
		}
		if (size == '4') {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -3.4E38
		} else if (size == '8') {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <-  -1.7E308
		} else { 
			stop("invalid datasize for a FLT (should be 4 or 8)") 
		}
	} else if (type == "INT") {
		raster@file@datatype <- 'integer'
		raster@file@datasigned <- signed
		raster@data@min <- round(minValue(raster))
		raster@data@max <- round(maxValue(raster))
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.integer(round(values(raster)))
		}
		if (size == '4') {
			raster@file@datasize <- as.integer(4)
			if (signed) {
				raster@file@nodatavalue <- -2147483647
			} else {
				raster@file@nodatavalue <- 4294967295
			}
		} else if (size == '2') {
			raster@file@datasize <- as.integer(2)
			if (signed) {
				raster@file@nodatavalue <- -32768
			} else {
				raster@file@nodatavalue <- 65535
			}
		} else if (size == '1') {
			raster@file@datasize <- as.integer(1)
			# there is no nodata value for byte
			raster@file@nodatavalue <- -9999
			warning("binary files of a single byte do not have NA values on disk")
		} else if (size == '8') {
			raster@file@datasize <- as.integer(8)
			if (signed) {
				raster@file@nodatavalue <- -9223372036854775808
			} else {
				raster@file@nodatavalue <- 18446744073709551615
			}
		} else {
			stop("invalid datasize for this datatype") 
		}
	} else if ( type == 'LOG' ) {
		raster@file@datatype <- 'logical'
		raster@file@datasigned <- TRUE		
		raster@file@datasize <- as.integer(1)
		raster@file@nodatavalue <- -127
	} else {
		stop("unknown datatype")
	} 
	return(raster)
}

