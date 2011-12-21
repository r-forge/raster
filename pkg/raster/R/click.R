# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009
# Version 1.0
# Licence GPL v3


if (!isGeneric("click")) {
	setGeneric("click", function(x, ...)
		standardGeneric("click"))
}	

setMethod('click', signature(x='missing'), 
	function(x, n=1, type="n", ...) {
		loc <- locator(n, type, ...)
		cbind(x=loc$x, y=loc$y)
	}
)
	
setMethod('click', signature(x='SpatialPolygons'), 
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		loc <- locator(n, type, ...)
		xyCoords <- cbind(x=loc$x, y=loc$y)
		if (id) {
			text(xyCoords, labels=1:n)
		}
	
		res <- overlay(object, SpatialPoints(xyCoords))
		if (xy) {
			res <- cbind(xyCoords, res)
		}
		if (is.matrix(res)) {
			rownames(res) <- 1:n
		}
		return(res)
	}
)



setMethod('click', signature(x='SpatialGrid'), 
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		x <- brick(x)
		click(x, n=n, id=id, xy=xy, type=type, ...)
	}
)

setMethod('click', signature(x='SpatialPixels'), 
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		x <- brick(x)
		click(x, n=n, id=id, xy=xy, type=type, ...)
	}
)

setMethod('click', signature(x='Raster'), 
	function(x, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
	
	loc <- locator(n, type, ...)
	xyCoords <- cbind(x=loc$x, y=loc$y)

	if (id) {
		text(xyCoords, labels=1:n)
	}
	
	cells <- cellFromXY(object, xyCoords)
	cells <- unique(na.omit(cells))
	if (length(cells) == 0 ) { stop('no valid cells selected') }
	xyCoords <- xyFromCell(object, cells)
	colnames(xyCoords) <- c('x', 'y')
	n <- nrow(xyCoords)

	value <- .cellValues(object, cells)

	if (nlayers(object) == 1)  {
		value <- matrix(value)
		colnames(value) <- layerNames(object)
	}
	
	if (cell) {
		value <- cbind(cells, value)
	}
	if (xy) { 
		value <- cbind(xyCoords, value)
	} 
	value
}
)

