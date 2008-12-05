# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

reclass <- function(raster, rclmat, filename="", overwrite=FALSE, INT=FALSE)  {
	if (class(raster) != 'RasterLayer' ) {
		stop('first two arguments should be objects of class "RasterLayer"')
	}

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
	if ( dataContent(raster) == 'all' |  dataContent(raster) == 'sparse') {
		for (i in 1:length(rclmat[,1])) {
			res <- values(raster)
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				res[ is.na(values(raster)) ] <- rclmat[i, 3] 
			} else { 
				res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
		}
		if ( dataContent(raster) == 'all') { outraster <- set.values(outraster, res) }
		if ( dataContent(raster) == 'sparse') { outraster <- set.values.row(outraster, res,  dataIndices(raster)) }
		if (filename(outraster) != "" ) {	outraster <- write.raster(outraster, overwrite=overwrite) }
	} else {
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			for (i in 1:length(rclmat[,1])) {
				res <- values(raster)
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(values(raster)) ] <- rclmat[i, 3] 
				} else {
					res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}	
			outraster <- set.values.row(outraster, res, r)
			outraster <- write.row(outraster, overwrite=overwrite)
		}	
	}	
	return(outraster)
}

