# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3

	


projectBbox <- function(object, projstring) {
	b <- getBbox(object)
	projstring <- projection(projstring)
	xy <- rbind(c(b@xmin, b@ymax), c(b@xmax, b@ymax), c(b@xmin, b@ymin), c(b@xmax, b@ymin))

	if (isLatLon(object)) {
		p <- project(xy, projstring, inv=FALSE)
	} else {
		p <- project(xy, projection(object), inv=TRUE)
		if (!isLatLon(projstring)) {
			p <- project(p, projstring, inv=FALSE)
		}
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


projectRaster <- function(from, to, method="ngb", filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1)  {
	validObject(to)
	validObject(projection(from, asText=F))
	validObject(projection(to, asText=F))
	if  (projection(from) == "NA") {stop("input projection is NA")}
	if  (projection(to) == "NA") {stop("output projection is NA")}
	
	pbb <- projectBbox(to, projection(from))
	bb <- intersectBbox(pbb, from)
	validObject(bb)


	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') 	}
	if (is.null(filename)){filename <- ""}
	to <- setRaster(to, filename)
	to <- setDatatype(to, datatype)

	rowCells <- 1:ncol(to)
	inMemory <- filename(to) == ""
	v <- vector(length=0)

	if (!canProcessInMemory(to, 1) && filename(to) == '') {
		filename <- tempfile()
		to <- setFilename(to, filename )
		if (options('verbose')[[1]]) { cat('writing raster to:', filename(to))	}
	}
	starttime <- proc.time()


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
		
		if (method=='ngb') {
			vals <- xyValues(from, xy)
		} else {
			vals <- xyValues(from, xy, method='bilinear')
		}
		
		vals <- xyValues(from, unProjXY)
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite)
		}
		
		if (r %in% track) { .showTrack(r, track, starttime) }
		
	}
	if (inMemory) {
		to <- setValues(to, v) 
		if (filename(to) != "") {
			to <- writeRaster(to, overwrite=overwrite)
		}
	}
	return(to)
}

