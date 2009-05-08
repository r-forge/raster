# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3

	


projectBbox <- function(object, projs) {
	b <- extent(object)
	projs <- projection(projs)
	xy <- rbind(c(b@xmin, b@ymax), c(b@xmax, b@ymax), c(b@xmin, b@ymin), c(b@xmax, b@ymin))

	if (isLatLon(object)) {
		p <- project(xy, projs, inv=FALSE)
	} else {
		p <- project(xy, projection(object), inv=TRUE)
		if (!isLatLon(projs)) {
			p <- project(p, projs, inv=FALSE)
		}
	} 

	xmin <- min(p[,1])
	xmax <- max(p[,1])
	ymin <- min(p[,2])
	ymax <- max(p[,2])	
	bb <- newBbox(xmin, xmax, ymin, ymax)
	obj <- setExtent(object, bb)
	projection(obj) <- projs
	return(obj)
}

.xyTransform <- function(crds, from, to) {
# This function was extracted from "spTransform.SpatialPoints" in the rgdal package
# Copyright (c) 2003-8 by Barry Rowlingson, Roger Bivand, and Edzer Pebesma
	res <- .Call("transform", from, to, nrow(crds), as.double(crds[,1]), as.double(crds[,2]), PACKAGE="rgdal")
	if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
		k <- which(!is.finite(res[[1]]) || !is.finite(res[[2]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]])[k,])
		stop(paste("failure in points", paste(k, collapse=":")))
	}
	return(cbind(res[[1]], res[[2]]))
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


	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') 	}
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
	v <- vector(length=0)

	starttime <- proc.time()
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		unProjXY <- .xyTransform(xy, projto, projfrom)
		
		if (method=='ngb') {
			vals <- xyValues(from, unProjXY)
		} else {
			vals <- xyValues(from, unProjXY, method='bilinear')
		}

		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite)
		}
		if (r %in% track) { .showTrack(r, to@nrows, track, starttime) }
	}
	if (inMemory) {
		to <- setValues(to, v) 
	}
	return(to)
}

