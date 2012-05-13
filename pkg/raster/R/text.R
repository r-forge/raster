# Author: Robert J. Hijmans
# Date : August 2010
# Version 0.9
# Licence GPL v3


if (!isGeneric("text")) {
	setGeneric("text", function(x, ...)
		standardGeneric("text"))
}	

setMethod('text', signature(x='RasterLayer'), 
	function(x, digits=0, fun=NULL, ...) {
		x <- rasterToPoints(x, fun=fun, spatial=FALSE)
		text(x[,1], x[,2], as.character(round(x[,3], digits=digits) ), ...)
	}
)

setMethod('text', signature(x='SpatialPolygons'), 
	function(x, labels, ...) {
		if (missing(labels)) {
			labels <- 1
		}
		
		if (length(labels)  == 1) {
			if (.hasSlot(x, 'data')) {
				if (labels %in% names(x)) {
					labels <- x@data[, labels]
				}
			} else {
				if (length(x)> 1) {
					labels <- 1:length(x)
				}
			}
			labels <- as.character(labels)
		}
		
		xy <- coordinates(x)		
		text(xy[,1], xy[,2], labels, ...)
	}
)


setMethod('text', signature(x='SpatialPoints'), 
	function(x, labels, ...) {

		if (missing(labels)) {
			labels <- 1
		}
		
		if (length(labels)  == 1) {
			if (.hasSlot(x, 'data')) {
				if (labels %in% names(x)) {
					labels <- x@data[, labels]
				}
			} else {
				if (length(x)> 1) {
					labels <- 1:length(x)
				}
			}
			labels <- as.character(labels)
		}

		xy <- coordinates(x)		
		text(xy[,1], xy[,2], labels, ...)
	}
)

