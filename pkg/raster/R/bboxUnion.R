# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3

unionBbox <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(getBbox(x))
	}
	bb <- getBbox(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- getBbox(objects[[i]])
		bb@xmin <- min(xmin(bb), xmin(bb2))
		bb@xmax <- max(xmax(bb), xmax(bb2))
		bb@ymin <- min(ymin(bb), ymin(bb2))
		bb@ymax <- max(ymax(bb), ymax(bb2))
	}
	return(bb)
}

intersectBbox <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(getBbox(x))
	}
	bb <- getBbox(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- getBbox(objects[[i]])
		bb@xmin <- max(xmin(bb), xmin(bb2))
		bb@xmax <- min(xmax(bb), xmax(bb2))
		bb@ymin <- max(ymin(bb), ymin(bb2))
		bb@ymax <- min(ymax(bb), ymax(bb2))
	}
	validObject(bb)
	return(bb)
}

