# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



projectBbox <- function(object, projstring) {
	b <- getBbox(object)
	projstring <- projection(projstring)
	xy <- rbind(c(b@xmin, b@ymax), c(b@xmax, b@ymax), c(b@xmin, b@ymin), c(b@xmax, b@ymin))

	if (isLatLon(projstring)) {
		p <- project(xy, projstring, inv=FALSE)
	} else {
		b <- project(xy, projection(object), inv=TRUE)
		p <- project(xy, projstring, inv=FALSE)
	}
	xmin <- min(p[,1])
	xmax <- max(p[,1])
	ymin <- min(p[,2])
	ymax <- max(p[,2])	
	bb <- newBbox(xmin, xmax, ymin, ymax)
	obj <- setExtent(object, bb)
	obj <- setProjection(obj, projstring)
	return(obj)
}


projectRaster <- function(from, to, method="nngb", overwrite=FALSE) {

# do the bounding boxes overlap at all? 

# get .innerbox first?

# are the projs not NA and valid and not the same?

	validObject(to)
	rowCells <- 1:ncol(to)
	inMemory <- filename(to) == ""
	v <- vector(length=0)
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		if (isLatLon(from)) {
			unProjXY <- project(xy, projection(to), inv=TRUE )
		} else {
			unProjXY <- project(xy, projection(from), inv=FALSE )
			if (!isLatLon(to)) {
				unProjXY <- project(unProjXY, projection(to), inv=TRUE )
			}
		}
		vals <- xyValues(from, unProjXY)
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite)
		}
	}
	if (inMemory) {
		to <- setValues(to, v) 
		if (filename(to) != "") {
			to <- writeRaster(to, overwrite=overwrite)
		}
	}
	return(to)
}

