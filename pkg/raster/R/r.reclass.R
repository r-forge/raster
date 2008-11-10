# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

r.reclass <- function(raster, rclmat, filename="", overwrite=FALSE, INT=FALSE)  {
	if ( is.null(dim(rclmat)) ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) 
	} else if ( dim(rclmat)[2] == 1 ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) }
	if ( dim(rclmat)[2] != 3 ) { stop('rclmat must have 3 columns') }
	colnames(rclmat) <- c("From", "To", "Becomes")	
	print(rclmat)
	outraster <- set.raster(raster, filename)
	if (INT) { 
		outraster <- set.datatype(outraster, "integer") 
		res <- vector(mode = "integer", length = ncol(raster))
	} else { 
		outraster <- set.datatype(outraster, "numeric") 
		res <- vector(mode = "numeric", length = ncol(raster))
	}
	if ( data.content(raster) == 'all' |  data.content(raster) == 'sparse') {
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				res[ is.na(values(raster)) ] <- rclmat[i, 3] 
			} else { 
				res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
		}
		if ( data.content(raster) == 'all') { outraster <- set.values(outraster, res) }
		if ( data.content(raster) == 'sparse') { outraster <- set.values.row(outraster, res,  data.indices(raster)) }
		if (filename(outraster) != "" ) {	outraster <- write.raster(outraster) }
	} else {
		for (r in 1:nrow(raster)) {
			raster <- read.row(raster, r)
			for (i in 1:length(rclmat[,1])) {
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(values(raster)) ] <- rclmat[i, 3] 
				} else if (is.na(rclmat[i,1]) == is.na(rclmat[i,2])) {
					res[ values(raster) == rclmat[i,1] ] <- rclmat[i , 3] 
				} else {
					res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}	
		}
		outraster <- set.values.row(outraster, res, r)
		outraster <- write.row(outraster, overwrite=overwrite)
	}	
	return(outraster)
}

