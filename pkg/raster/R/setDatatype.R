# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

setDatatype <- function(raster, datatype) {
# for backward compatibility issues and non fatal mistakes.
	datatype <- substr( toupper( trim(datatype) ), 1, 5)
	if (datatype=='LOGIC') {datatype <- 'LOG1S'}
	if (datatype == 'INTEG') {datatype <- 'INT4S'}
	if (datatype == 'NUMER') {datatype <- 'FLT4S'}
	if (datatype == 'FLOAT') {datatype <- 'FLT4S'}
	if (datatype == 'DOUBL') {datatype <- 'FLT8S'}
	if (datatype == 'SINGL') {datatype <- 'FLT4S'}		
	if (datatype == 'REAL') {datatype <- 'FLT4S'}	
	
	if (nchar(datatype) < 3) {
		stop(paste('invalid datatype:', datatype))
	} else if (nchar(datatype) == 3) {
		if (datatype == 'LOG') { 
			datatype <- paste(datatype, '1S', sep='') 		
		} else {
			datatype <- paste(datatype, '4S', sep='') 
		}
	} else if (nchar(datatype) == 4) {
		if (datatype == 'INT1') { 
			datatype <- paste(datatype, 'U', sep='') 
		} else { 
			datatype <- paste(datatype, 'S', sep='')
		}
	}

# now for real
	
	if (!(substr(datatype, 1, 4) %in% c('LOG1', 'INT1', 'INT2', 'INT4', 'INT8', 'INT1', 'INT2', 'INT4', 'INT8', 'FLT4', 'FLT8'))) {
		stop('not a valid data type')
	}
	type <- substr(datatype,1,3)
	size <- substr(datatype,4,4)
	signed <- substr(datatype,1,3) != 'U'
	
	if (type == "FLT") {
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.numeric(values(raster))
		}
		if (size == '4') {
			raster@file@datanotation <- 'FLT4S'
			raster@file@nodatavalue <- -3.4E38
		} else if (size == '8') {
			raster@file@datanotation <- 'FLT8S'
			raster@file@nodatavalue <-  -1.7E308
		} else { 
			stop("invalid datasize for a FLT (should be 4 or 8)") 
		}
	} else if (type == "INT") {
		raster@data@min <- round(minValue(raster))
		raster@data@max <- round(maxValue(raster))
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.integer(round(values(raster)))
		}
		if (size == '4') {
			if (signed) {
				raster@file@datanotation <- 'INT4S'
				raster@file@nodatavalue <- -2147483647
			} else {
				raster@file@datanotation <- 'INT4U'
				raster@file@nodatavalue <- 4294967295
			}
		} else if (size == '2') {
			if (signed) {
				raster@file@datanotation <- 'INT2S'
				raster@file@nodatavalue <- -32768
			} else {
				raster@file@datanotation <- 'INT2U'
				raster@file@nodatavalue <- 65535
			}
		} else if (size == '1') {
			# there is no nodata value for byte
			raster@file@nodatavalue <- -9999
			if (signed) {
				raster@file@datanotation <- 'INT1S'
			} else {
				raster@file@datanotation <- 'INT1U'
			}
			warning("binary files of a single byte do not have NA values on disk")
		} else if (size == '8') {
			if (signed) {
				raster@file@nodatavalue <- -9223372036854775808
				raster@file@datanotation <- 'INT8S'							
			} else {
				raster@file@nodatavalue <- 18446744073709551615
				raster@file@datanotation <- 'INT8U'			
			}
		} else {
			stop("invalid datasize for this datatype") 
		}
	} else if ( type == 'LOG' ) {
		raster@file@nodatavalue <- -127
		raster@file@datanotation <- 'LOG1S'
	} else {
		stop("unknown datatype")
	} 
	return(raster)
}

