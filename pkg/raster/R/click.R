# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3




click <- function(object, n=1, id=FALSE, xy=FALSE, type="n", ...) {
	loc <- locator(n, type, ...)
	xy <- cbind(loc$x, loc$y)
	if (missing(object)) { return(xy) }
	cells <- cellFromXY(object, xy)
	cells <- unique(na.omit(cells))
	if (length(cells) == 0 ) { stop('no valid cells selected') }
	xy <- xyFromCell(object, cells)
	colnames(xy) <- c('x', 'y')
	n <- nrow(xy)
	if (id) {
		for (i in 1:n) {
			text(xy[i,1], xy[i,2], i)
		}
	}

	if (dataContent(object) != 'all') {
		value <- xyValues(object, xy)
	} else {
		cell <- cellFromXY(object, xy)
		if (class(object) == 'RasterStack') {
			value <- values(object)[cell,]
		} else {
			value <- values(object)[cell]
		}
	}	
	if (class(object) == 'RasterStack') {
		value <- t(matrix(value, nrow=n))
		rownames(value) <- layerNames(object)
	} else {
		value <- t(matrix(value))
		if (layerNames(object) == "") {
			rownames(value) <- 'value'
		} else {
			rownames(value) <- layerNames(object)
		}
	}
	
	if (xy) { 
		value <- rbind(t(xy), value)
	} 
	return(t(value))
	
}
