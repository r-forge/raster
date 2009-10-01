# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

unionExtent <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	bb <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- extent(objects[[i]])
		bb@xmin <- min(xmin(bb), xmin(bb2))
		bb@xmax <- max(xmax(bb), xmax(bb2))
		bb@ymin <- min(ymin(bb), ymin(bb2))
		bb@ymax <- max(ymax(bb), ymax(bb2))
	}
	return(bb)
}

intersectExtent <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	bb <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- extent(objects[[i]])
		bb@xmin <- max(xmin(bb), xmin(bb2))
		bb@xmax <- min(xmax(bb), xmax(bb2))
		bb@ymin <- max(ymin(bb), ymin(bb2))
		bb@ymax <- min(ymax(bb), ymax(bb2))
	}
	validObject(bb)
	return(bb)
}

