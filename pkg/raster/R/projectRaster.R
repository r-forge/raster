# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


projectExtent <- function(object, projs) {
	validObject(projection(object, asText=FALSE))
	validObject(projection(projs, asText=FALSE))
	projfrom <- projection(object)
	projto <- projection(projs)
	b <- extent(object)
	xy <- rbind(c(b@xmin, b@ymax), c(b@xmax, b@ymax), c(b@xmin, b@ymin), c(b@xmax, b@ymin))
	res <- .Call("transform", projfrom, projto, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
	p <- cbind(res[[1]], res[[2]])
	if (any(is.infinite(p[,1])) || any(is.infinite(p[,2]))) {
		stop("non finite transformation detected")
	}
	obj <- raster(newBbox(min(p[,1]), max(p[,1]), min(p[,2]),  max(p[,2])), nrows=nrow(object), ncols=ncol(object), proj=(projs))
	return(obj)
}


projectRaster <- function(from, to, method="ngb", filename="", filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1)  {
	if (dataContent(from) != 'all' & dataSource(from) == 'ram') { stop('no vales for "from". Nothing to do') }

	validObject(to)
	validObject(projection(from, asText=FALSE))
	validObject(projection(to, asText=FALSE))
	projfrom <- projection(from)
	projto <- projection(to)
	if (projfrom == "NA") {stop("input projection is NA")}
	if (projto == "NA") {stop("output projection is NA")}
	
	pbb <- projectExtent(to, projection(from))
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
	inMemory <- filename(to) == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncol(to), ncol=nrow(to))
	}
	
	if (method=='ngb') {
		xymethod <- 'simple' 
	} else {
		xymethod <- 'bilinear' 	
	}
	
	starttime <- proc.time()
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		res <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
		unProjXY <- cbind(res[[1]], res[[2]])
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


