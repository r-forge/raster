# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

 
 
writeRaster <- function(raster, filetype='raster', filename='', overwrite=FALSE, assign=FALSE) {
	if (assign){ 
		raster_name <- deparse(substitute(raster))
	}

	if (filename != '') {
		filename(raster) <- filename
	}
	if (filename(raster) == '') {
		stop('RasterLayer has no filename; and no filename specified as argument to writeRaster')
	}
	
	if (! dataContent(raster) %in% c('row', 'rows', 'all', 'sparse') ) {
		stop('No usable data available for writing.')
	}
	
	if (filetype=='raster') {
		if (substr(dataContent(raster), 1, 3) == 'row' ) {
			raster <- .writeRasterRow(raster, overwrite=overwrite)
		} else {
			raster <- .writeRasterAll(raster, overwrite=overwrite)
		}  
	} else if (filetype=='ascii') {
		raster <- .writeAscii(raster, overwrite=overwrite)
		
	} else if (filetype=='CDF') {
		raster <- .writeRasterCDF(raster, overwrite=overwrite)
		
	} else { 
		.isSupportedGDALFormat(filetype)
		if (dataContent(raster) == 'row' ) {
			raster <- .writeGDALrow(raster, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		} else {
			raster <- .writeGDALall(raster, gdalfiletype=filetype, overwrite=overwrite, mvFlag=NA, options=NULL)
		}  
	}
	if (assign) {
		assign(raster_name, raster, envir=parent.frame())
		return(invisible())	
	} else {
		return(raster)
	}
}	

