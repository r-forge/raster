

newBrick <- function(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmn, xmx, ymn, ymx)
	return(brickFromBbox(bb, nrows=nrows, ncols=ncols, projstring))
}

brickFromBbox <- function(boundingbox, nrows=1, ncols=1, projstring="") {
	nr = as.integer(round(nrows))
	nc = as.integer(round(ncols))
	if (nc < 1) { stop("ncols should be larger than 0") }
	if (nr < 1) { stop("nrows should be larger than 0") }
	proj4string <- newCRS(projstring)
	if (validObject(boundingbox)) {
		brick <- new("RasterBrick", bbox = boundingbox@bbox, proj4string=proj4string, ncols = nc, nrows = nr )
		brick@data@content <- 'nodata'
		return(brick) 
	} else {
		return <- NA 
	}
}

brickFromStack <- function(stack, values=FALSE) {
	stop("not implemented yet")
}


brickFromFile <- function(filename, values=FALSE) {
	stop("not implemented yet")
}
