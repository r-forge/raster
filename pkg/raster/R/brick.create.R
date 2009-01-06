

newBrick <- function(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmn, xmx, ymn, ymx)
	return(brickFromBbox(bb, nrows=nrows, ncols=ncols, projstring))
}

brickFromBbox <- function(bndbox, nrows=1, ncols=1, projstring="") {
	nrows = as.integer(round(nrows))
	ncols = as.integer(round(ncols))
	if (ncols < 1) { stop("ncols should be > 0") }
	if (nrows < 1) { stop("nrows should be > 0") }
	proj4string <- newCRS(projstring)
	brick <- new("RasterBrick", bbox = bndbox@bbox, proj4string=proj4string, ncols = ncols, nrows = nrows )
	return(brick) 
}

brickFromStack <- function(stack) {
	stop("not implemented yet")
}

brickFromFile <- function(filename, values=FALSE) {
	stop("not implemented yet")
}
