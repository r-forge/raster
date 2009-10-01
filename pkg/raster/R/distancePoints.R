# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


distanceFromPoints <- function(object, xy, filename="", ...) {

	pts <- .pointsToMatrix(xy)
	rm(xy)

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

	filetype <- 'raster'
	overwrite <- TRUE
	
	if (isLatLon(object)) { disttype <- 'GreatCircle' } else { disttype <- 'Eucledian' }
	
	
	rst <- raster(object)
	if (!canProcessInMemory(rst, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}
	filename(rst) <- filename

	x <- xFromCol(rst, 1:ncol(rst))
	arow <- rep(NA, ncol(rst))
	v <- vector()
	
	starttime <- proc.time()
	pb <- .setProgressBar(nrow(rst), type=.progress(...))
	for (r in 1:nrow(rst)) {	
		vals <- arow
		y <- yFromRow(rst, r)
		xy <- cbind(x, y)
		for (c in 1:length(xy[,1])) {
			vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
		}
		if (rst@file@name == "") {
			v <- c(v, vals)
		} else {
			rst <- setValues(rst, vals, r)
			rst <- writeRaster(rst, overwrite=overwrite, filetype=filetype)
		}
		.doProgressBar(pb, r) 	
	}	
	.closeProgressBar(pb, starttime)
	if (rst@file@name == "") { 
		rst <- setValues(rst, v) 
	}
	return(rst)
}


