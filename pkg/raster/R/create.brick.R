

brick.new <- function(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projection="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmn, xmx, ymn, ymx, projection)
	return(brick.from.bbox(bb, nrows=nrows, ncols=ncols))
}

brick.from.bbox <- function(boundingbox, nrows=1, ncols=1) {
	nr = as.integer(round(nrows))
	nc = as.integer(round(ncols))
	if (nc < 1) { stop("ncols should be larger than 0") }
	if (nr < 1) { stop("nrows should be larger than 0") }
	if (validObject(boundingbox)) {
		brick <- new("RasterBrick", bbox = boundingbox@bbox, proj4string=boundingbox@proj4string, ncols = nc, nrows = nr )
		brick@data@content <- 'nodata'
		return(brick) 
	} else {
		return <- NA 
	}
}

brick.from.stack <- function(stack, values=FALSE) {
	stop("not implemented yet")
}


brick.from.file <- function(filename, values=FALSE) {
	stop("not implemented yet")
}
