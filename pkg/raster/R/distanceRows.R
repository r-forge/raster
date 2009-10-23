# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


.rasterToPointsChunk <- function(object, startrow, endrow) {
	object <- readRows(object, startrow, nrows=(endrow-startrow)+1)
	c1 <- cellFromRowCol(object, startrow, 1)
	c2 <- cellFromRowCol(object, endrow, ncol(object))
	xyv <- cbind(xyFromCell(object, c1:c2), values(object))
	return( subset(xyv, !(is.na(xyv[,3]))) )
}



.distanceRows <- function(object, filename, ...) {

	nrows <- min(100, floor(nrow(object)/2))  # arbitrary right now...
	chunks <- ceiling(nrow(object) / nrows)
	
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	
	if( (!overwrite) & file.exists(filename)) {
		stop('file exists; use overwrite=TRUE to overwrite it')
	}

	if (isLatLon(object)) { disttype <- 'GreatCircle' } else { disttype <- 'Euclidean' }
	
	rst1 <- raster(object, filename=rasterTmpFile())
	rst2 <- raster(object, filename=rasterTmpFile())
	
	x <- xFromCol(rst1, 1:ncol(rst1))
	arow <- rep(NA, ncol(rst1))
	v <- vector()
	
	pb <- pbCreate(nrow(rst1) * chunks, type=.progress(...))

	for (k in 1:chunks) {
		startrow <- (k-1)*nrows+1
		if (!validRow(rst1, startrow)) { break }  # end reached
		endrow <- min(startrow+nrows-1, nrow(rst1))
		pts <- .rasterToPointsChunk(object, startrow, endrow)[,1:2] 
		if (length(pts) == 0) {
			for (r in 1:nrow(rst1)) {	
				if (k==1) {
					rst1 <- setValues(rst1, arow, r)
					rst1 <- writeRaster(rst1, filename(rst1), datatype=datatype, overwrite=TRUE, filetype='raster')
				} else {
					rst2 <- readRow(rst2, r)
					rst1 <- setValues(rst1, values(rst2), r)
					rst1 <- writeRaster(rst1, filename(rst1), datatype=datatype, overwrite=TRUE, filetype='raster')			
				}	
				pbStep(pb, r) 	
			}
		} else {
			for (r in 1:nrow(rst1)) {	
				vals <- arow
				y <- yFromRow(rst1, r)
				xy <- cbind(x, y)
				for (c in 1:length(xy[,1])) {
					vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
				}
				if (k==1) {
					rst1 <- setValues(rst1, vals, r)
					rst1 <- writeRaster(rst1, filename(rst1), datatype=datatype, overwrite=TRUE, filetype='raster')
				} else {
					rst2 <- readRow(rst2, r)
					vals <- pmin(values(rst2), vals)
					rst1 <- setValues(rst1, vals, r)
					rst1 <- writeRaster(rst1, filename(rst1), datatype=datatype, overwrite=TRUE, filetype='raster')			
				}
			pbStep(pb, r) 	
			}
		}
		tmp <- rst2
		rst2 <- rst1
		rst1 <- tmp
	}	
	pbClose(pb)
    return(rst2)
	return( saveAs(rst2, filename=filename, filetype=.filetype(...), datatype=datatype, overwrite=overwrite, progress=.progress(...)) )
}

