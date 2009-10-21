# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

reclass <- function(raster, rclmat, update=FALSE, filename="", ...)  {
	if (is.null(filename)) { filename <- "" }
	
	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

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
	
	if (getOption('verbose')) {
		print(rclmat)
	}
	
	if (!canProcessInMemory(raster, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename(outRaster))	}						
	}
	
	outRaster <- raster(raster, filename=filename)
	dataType(outRaster) <- datatype

	res <- vector(length = ncol(raster))
	
	
	if ( filename == "" ) {
		if (dataContent( raster ) != 'all') {
			raster <- readAll(raster)
		}
		res <- values(raster)
		if (update) {
			for (i in 1:length(rclmat[,1])) {
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(res) ] <- rclmat[i, 3] 
				} else { 
					res[ (res >= rclmat[i,1]) & (res <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}
		} else {
			for (i in 1:length(rclmat[,1])) {
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(values(raster)) ] <- rclmat[i, 3] 
				} else { 
					res[ (values(raster) >= rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}
		}
		
		return( setValues(outRaster, res) )
		
	} else {

		hasNA <- FALSE
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				namat <- rclmat[i,]
				rclmat <- rclmat[-i,]
				hasNA <- TRUE
			}
		}

		starttime <- proc.time()
		pb <- pbSet(nrow(raster), type=.progress(...))
		if (update) {
			for (r in 1:nrow(raster)) {
				res <- getValues(raster, r)
				for (i in 1:length(rclmat[,1])) {
					res[ (res >= rclmat[i,1]) & (res <= rclmat[i,2]) ] <- rclmat[i,3] 
				}
				if (hasNA) {
					res[ is.na(res) ] <- namat[1, 3] 				
				}	
				outRaster <- setValues(outRaster, res, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
				pbDo(pb, r)
			}
		} else {
			for (r in 1:nrow(raster)) {
				res <- getValues(raster, r)
				for (i in 1:length(rclmat[,1])) {
					res[ (res >= rclmat[i,1]) & ( res <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
				if (hasNA) {
					res[ is.na(res) ] <- namat[1, 3] 				
				}	
				outRaster <- setValues(outRaster, res, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
				pbDo(pb, r)
			}
			pbClose(pb, starttime)
		}
		return(outRaster)
	}
}

