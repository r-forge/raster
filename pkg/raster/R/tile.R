# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


tileSize <- function(x) {
	size <- 10  # for now
	nb <- ceiling(x@nrows / size)
	return(data.frame(size, nb))
}

readTile <- function(x, bs, b) {
	startrow <- (b-1) * bs$size + 1
	endrow <- min(nrow(raster), startrow+nrows-1)
	nrows <- endrow-startrow+1
	cols <- ncol(raster) * nrows
	raster <- .rasterRead(raster, startrow, 1, cols)
	raster@data@content <- 'tile' 
	firstcell <- cellFromRowCol(raster, startrow, 1)
	lastcell <- cellFromRowCol(raster, endrow, ncol(raster))
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}
