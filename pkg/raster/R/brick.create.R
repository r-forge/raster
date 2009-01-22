

newBrick <- function(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmn, xmx, ymn, ymx)
	return(brickFromBbox(bb, nrows=nrows, ncols=ncols, projstring))
}

brickFromBbox <- function(bndbox, nrows=1, ncols=1, projstring="") {
	nrows = as.integer(round(nrows))
	ncols = as.integer(round(ncols))
	if (ncols < 1) { stop("ncols should be > 0") }
	if (nrows < 1) { stop("nrows should be > 0") }
	crs <- newCRS(projstring)
	brick <- new("RasterBrick", bbox = getBbox(bndbox), crs=crs, ncols = ncols, nrows = nrows )
	return(brick) 
}

brickFromStack <- function(stack) {
	stop("not implemented yet")
}

brickFromFile <- function(filename, values=FALSE) {
	stop("not implemented yet")
}

setGeneric("makeBrick", function(object, ...) standardGeneric("makeBrick"))

setMethod('makeBrick', signature(object='RasterLayer'), function(object, ...)
	{ 
		RLlist <- list(...)
		if (dataContent(object) != 'all'){stop("rasterBricks can only be created from RasterLayers with all data in memory")}
		for (i in seq(along=object))
		{
			if (as(RLlist[[i]],"BasicRaster") != as(RLlist[[i]],"BasicRaster")){stop("RasterLayer objects are not equal in extent, resolution and/or projection")}
			if (class(RLlist[[i]]) != "RasterLayer"){stop("function only implemented for objects of same class")}
			if (dataContent(RLlist[[i]]) != 'all'){stop("rasterBricks can only be created when all RasterLayer data are in memory")}
		}
		datavalues <- matrix(nrow=ncell(object),ncol=1+length(RLlist))
		datavalues[,1] <- values(object)
		for (j in seq(along=object))
		{
			datavalues[,j+1] <- values(RLlist[[j]])		
		}
		brick <- new("RasterBrick", bbox = getBbox(object), crs=projection(object, asText = FALSE), ncols = ncol(object), nrows = nrow(object))
		brick@data@values <- datavalues
		brick@data@nlayers <- as.integer(1+length(RLlist))
		brick@data@content <- "all"
		return(brick)
	}
)

#makeBrick should also be defined for signature Bbox, RasterStack, and even filename  could be considered.
