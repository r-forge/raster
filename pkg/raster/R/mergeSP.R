# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



setMethod('merge', signature(x='Spatial', y='data.frame'), 
function(x, y, by=intersect(names(x), names(y)), by.x=by, by.y=by, all.x=TRUE, suffixes = c(".x",".y"), incomparables = NULL, ...) {
	if (!'data' %in% slotNames(x)) {
		stop('x has no data.frame')
	}
	d <- x@data
	d$donotusethisvariablename976 <- 1:nrow(d)

	y <- unique(y)
	if (!all.x) {
		y$donotusethisvariablename679 <- 1
	}
	d <- merge(d, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, incomprables=incomprable, all.x=TRUE, all.y=FALSE)
	d <- d[order(d$donotusethisvariablename976), ]
	d <- d[, -which(colnames(d)=='donotusethisvariablename976'), drop=FALSE]
	rownames(d) <- row.names(x)
	x@data <- d
	if (! all.x ) {
		x <- x[!is.na(x@data$donotusethisvariablename679),  ,drop=FALSE] 
		x@data <- x@data[ , -which(colnames(x@data)=='donotusethisvariablename679'), drop=FALSE]
	}
	x
} 
)


setMethod('merge', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y, ..., intersect=TRUE) {

	require(rgeos)

	if (!intersect) {
		return( .appendPolygons(x, y, ...) )
	}
	
	row.names(x) <- as.character(1:length(row.names(x)))
		
	yy <- list(...)
	if (length(yy) > 0) {
		keep <- sapply(yy, function(x) inherits(a, 'SpatialPolygons'))
		yy <- yy[keep]
	}
	yy <- c(y, yy)
	
	for (y in yy) {

		if (! identical(projection(x), projection(y)) ) {
			warning('non identical CRS')
			y@proj4string <- x@proj4string
		}	
	
		subs <- gIntersects(x, y, byid=TRUE)
		if (sum(subs)==0) {
			x <- .appendPolygons(x, y)
			next
		}
		y <- spChFIDs(y, as.character(1:length(row.names(y))))

		
		dat <- NULL
		xdata <- .hasSlot(x, 'data')
		ydata <- .hasSlot(y, 'data')

		res <- NULL
		yd <- aggregate(y)
		dif1 <- gDifference(x, yd, byid=TRUE)
		if (!is.null(dif1) & xdata) {
			ids <- as.numeric(sapply(row.names(dif1), function(x) strsplit(x, ' ')[[1]][1]))
			dif1 <- SpatialPolygonsDataFrame(dif1, x@data[ids,])
		}

		xd <- aggregate(x)
		dif2 <- gDifference(y, xd, byid=TRUE)
		if (!is.null(dif2) & ydata) {
			ids <- sapply(row.names(dif2), function(x) strsplit(x, ' ')[[1]][1])
			dif2 <- SpatialPolygonsDataFrame(dif2, y@data[ids,])
		}

		int <- intersect(x[subsx,], y[subsy,])
		if (!is.null(dif1) | !is.null(dif2)) {
			x <- .appendPolygons(dif1, int, dif2, ...) 
		} else {
			x <- int
		}
	}
	x	
}
)

