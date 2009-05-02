# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

reclass <- function(raster, rclmat, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)  {

	if (class(raster) != 'RasterLayer' ) {
		stop('first argument should be an object of class "RasterLayer"')
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
	
	if (dataContent(raster) == 'all') { nr <- 1 } else { nr <- 2 }
	if (!canProcessInMemory(raster, nr) && filename == '') {
		filename <- rasterTmpFile()
		if (options('verbose')[[1]]) { cat('writing raster to:', filename(outRaster))	}						
	}
	
	outRaster <- raster(raster)
	filename(outRaster) <- filename
	dataType(outRaster) <- datatype

	res <- vector(length = ncol(raster))
	
	if ( dataContent(raster) == 'all' |  dataContent(raster) == 'sparse') {
		res <- values(raster)
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				res[ is.na(values(raster)) ] <- rclmat[i, 3] 
			} else { 
				res[ (values(raster) >= rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
		}
		if ( dataContent(raster) == 'all') { 
			outRaster <- setValues(outRaster, res) 
		}
		if ( dataContent(raster) == 'sparse') { 
			outRaster <- setValues(outRaster, res,  dataIndices(raster)) 
		}
		if (outRaster@file@name != "" ) {
			outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		starttime <- proc.time()
		hasNA <- FALSE
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				namat <- rclmat[i,]
				rclmat <- rclmat[-i,]
				hasNA <- TRUE
			}
		}
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			res <- values(raster)
			for (i in 1:length(rclmat[,1])) {
				res[ (values(raster) >= rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
			if (hasNA) {
				res[ is.na(values(raster)) ] <- namat[1, 3] 				
			}	
			outRaster <- setValues(outRaster, res, r)
			outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
		}
		if (r %in% track) { .showTrack(r, outRaster@nrows, track, starttime) }
	}	
	return(outRaster)
}

