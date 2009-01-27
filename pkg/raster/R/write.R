
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3




rasterWriteFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	short <- c("raster", "ascii", as.vector(gd[,1]))
	long <- c("raster package format", "Arc ascii", as.vector(gd[,2]))
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}


.isSupportedGDALFormat <- function(dname) {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	res <- dname %in% gd
	if (!res) { stop(paste(dname, "is not a supported file format. See rasterWriteFormats()" ) ) }
	return(res)
}

 
 
writeRaster <- function(raster, format='raster', overwrite=FALSE) {
	
	if (dataContent(raster) != 'row' & dataContent(raster) != 'all' & dataContent(raster) != 'sparse' ) {
		stop('No data available for writing. First use setValues()')
	}

	if (format=='raster') {
		if (dataContent(raster) == 'row' ) {
			raster <- .writeRasterRow(raster, overwrite=overwrite)
		} else {
			raster <- .writeRasterAll(raster, overwrite=overwrite)
		}  
	} else if (format=='ascii') {
		raster <- .writeAscii(raster, overwrite=overwrite)
	} else { 
		.isSupportedGDALFormat(format)
		if (dataContent(raster) == 'row' ) {
			raster <- .writeGDALrow(raster, format, overwrite, asInt=FALSE, mvFlag=NA, options=NULL)
		} else {
			raster <- .writeGDALall(raster, format, overwrite, asInt=FALSE, mvFlag=NA, options=NULL)
		}  
	}
	return(raster)
}	

writeStack <- function(rstack, overwrite=FALSE) {
	stop("not available yet")
	for (i in 1:nlayers(rstack)) {
	
	}
}
