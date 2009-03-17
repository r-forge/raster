# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.isSupportedGDALFormat <- function(dname) {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	res <- dname %in% gd
	if (!res) { stop(paste(dname, "is not a supported file format. See writeFormats()" ) ) }
	return(res)
}


#.GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', '
# what are these?  CInt16', 'CInt32',   'CFloat32', 'CFloat64')	
# this needs to get fancier; depending on object and the abilties of the drivers
.getGdalDType <- function(dtype) {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'INT4U', 'INT8U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8
	if (type == 'LOG') {
		return('Byte')
	}
	if (type == 'INT') { 
		type <- 'Int' 
		if (size == 64) {
			size <- 32
			warning('8 byte integer values not supported by rgdal, changed to 4 byte integer values')
		}
		if (size == 8) {
			return('Byte')
		} else if (dataSigned(dtype)) {
			type <- paste('U', type, sep='')
		}
	} else { 
		type <- 'Float' 
	}
	return(paste(type, size, sep=''))
}

