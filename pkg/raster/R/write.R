
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


writeFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	short <- c("raster", "ascii", as.vector(gd[,1]))
	long <- c("raster package format", "Arc ascii", as.vector(gd[,2]))
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}

 
 
writeRaster <- function(raster, filetype='raster', overwrite=FALSE) {
	
	raster_name <- deparse(substitute(raster))
	
	if (dataContent(raster) != 'row' & dataContent(raster) != 'all' & dataContent(raster) != 'sparse' ) {
		stop('No data available for writing. First use setValues()')
	}
	
	if (filetype=='raster') {
		if (dataContent(raster) == 'row' ) {
			raster <- .writeRasterRow(raster, overwrite=overwrite)
		} else {
			raster <- .writeRasterAll(raster, overwrite=overwrite)
		}  
	} else if (filetype=='ascii') {
		raster <- .writeAscii(raster, overwrite=overwrite)
	} else { 
		.isSupportedGDALFormat(filetype)
		if (dataContent(raster) == 'row' ) {
			raster <- .writeGDALrow(raster, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		} else {
			raster <- .writeGDALall(raster, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		}  
	}
	
	assign(raster_name, raster, envir=parent.frame())
#	return(invisible())	
	
	return(raster)
	
}	

writeStack <- function(rstack, overwrite=FALSE) {
	stop("not available yet")
	for (i in 1:nlayers(rstack)) {
		
	}
}
