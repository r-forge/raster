# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

reclass <- function(raster, rclmat, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)  {
	if (class(raster) != 'RasterLayer' ) {
		stop('first two arguments should be objects of class "RasterLayer"')
	}

	if ( is.null(dim(rclmat)) ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) 
	} else if ( dim(rclmat)[2] == 1 ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) 
	}
	if ( dim(rclmat)[2] != 3 ) { stop('rclmat must have 3 columns') }
	colnames(rclmat) <- c("From", "To", "Becomes")	
	
	if (options('verbose')[[1]]) {
		print(rclmat)
	}
	
	outraster <- raster(raster, filename)
	dataType(outraster) <- datatype

	res <- vector(length = ncol(raster))
	
	if ( dataContent(raster) == 'all' |  dataContent(raster) == 'sparse') {
		res <- values(raster)
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				res[ is.na(values(raster)) ] <- rclmat[i, 3] 
			} else { 
				res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
		}
		if ( dataContent(raster) == 'all') { 
			outraster <- setValues(outraster, res) 
		}
		if ( dataContent(raster) == 'sparse') { 
			outraster <- setValues(outraster, res,  dataIndices(raster)) 
		}
		if (outraster@file@name != "" ) {
			writeRaster(outraster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		starttime <- proc.time()

		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			res <- values(raster)
			for (i in 1:length(rclmat[,1])) {
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(values(raster)) ] <- rclmat[i, 3] 
				} else {
					res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}	
			outraster <- setValues(outraster, res, r)
			writeRaster(outraster, overwrite=overwrite, filetype=filetype)
		}
		if (r %in% track) { .showTrack(r, outraster@nrows, track, starttime) }
	}	
	return(outraster)
}

