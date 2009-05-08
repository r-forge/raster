# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.xyTransform <- function(crds, projfrom, projto) {
# May 2009. This function was extracted from function "spTransform.SpatialPoints" in the rgdal package
# Copyright (c) 2003-8 by Barry Rowlingson, Roger Bivand, and Edzer Pebesma
	res <- .Call("transform", projfrom, projto, nrow(crds), as.double(crds[,1]), as.double(crds[,2]), PACKAGE="rgdal")
	if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
		k <- which(!is.finite(res[[1]]) || !is.finite(res[[2]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]])[k,])
		stop(paste("failure in points", paste(k, collapse=":")))
	}
	return(cbind(res[[1]], res[[2]]))
}



projectBbox <- function(object, projs) {
	validObject(projection(object, asText=FALSE))
	validObject(projection(projs, asText=FALSE))
	projfrom <- projection(object)
	projto <- projection(projs)
	b <- extent(object)
	xy <- rbind(c(b@xmin, b@ymax), c(b@xmax, b@ymax), c(b@xmin, b@ymin), c(b@xmax, b@ymin))
	p <- .xyTransform(xy, projfrom, projto)	
	obj <- setExtent(object, newBbox(min(p[,1]), max(p[,1]), min(p[,2]),  max(p[,2])))
	projection(obj) <- projs
	return(obj)
}


projectRaster <- function(from, to, method="ngb", filename="", filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1)  {
	validObject(to)
	validObject(projection(from, asText=FALSE))
	validObject(projection(to, asText=FALSE))
	projfrom <- projection(from)
	projto <- projection(to)
	if (projfrom == "NA") {stop("input projection is NA")}
	if (projto == "NA") {stop("output projection is NA")}
	
	pbb <- projectBbox(to, projection(from))
	bb <- intersectBbox(pbb, from)
	validObject(bb)

	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') }
	filename <- trim(filename)
	to <- raster(to)
	filename(to) <- filename
	dataType(to) <- datatype

	rowCells <- 1:ncol(to)

	if (!canProcessInMemory(to, 1) && filename(to) == "") {
		filename <- rasterTmpFile()
		filename(to) <- filename
		if (getOption('verbose')) { cat('writing raster to:', filename(to))	}
	}
	inMemory <- to@file@name == ""
	v <- matrix(NA, nrow=ncol(to), ncol=nrow(to))
	
	if (method=='ngb') {
		xymethod <- 'simple' 
	} else {
		xymethod <- 'bilinear' 	
	}
	
	starttime <- proc.time()
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		unProjXY <- .xyTransform(xy, projto, projfrom)
		vals <- xyValues(from, unProjXY, method=xymethod)
		if (inMemory) {
			v[,r] <- vals
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite)
		}
		if (r %in% track) { .showTrack(r, to@nrows, track, starttime) }
	}
	if (inMemory) {
		to <- setValues(to, as.vector(v))
	}
	return(to)
}

