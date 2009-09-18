# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

reclass <- function(raster, rclmat, update=FALSE, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)  {
	if (is.null(filename)) { filename <- "" }

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
	
	outRaster <- raster(raster, filename=filename, datatype=datatype)

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
		starttime <- proc.time()
		hasNA <- FALSE
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				namat <- rclmat[i,]
				rclmat <- rclmat[-i,]
				hasNA <- TRUE
			}
		}

		if (update) {
			for (r in 1:nrow(raster)) {
				res <- valuesRow(raster, r)
				for (i in 1:length(rclmat[,1])) {
					res[ (res >= rclmat[i,1]) & (res <= rclmat[i,2]) ] <- rclmat[i,3] 
				}
				if (hasNA) {
					res[ is.na(res) ] <- namat[1, 3] 				
				}	
				outRaster <- setValues(outRaster, res, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
			}
			if (r %in% track) { .showTrack(r, outRaster@nrows, track, starttime) }
			
		} else {
			for (r in 1:nrow(raster)) {
				res <- valuesRow(raster, r)
				for (i in 1:length(rclmat[,1])) {
					res[ (res >= rclmat[i,1]) & ( res <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
				if (hasNA) {
					res[ is.na(res) ] <- namat[1, 3] 				
				}	
				outRaster <- setValues(outRaster, res, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
			}
			if (r %in% track) { .showTrack(r, outRaster@nrows, track, starttime) }
		}
		return(outRaster)
	}
}

