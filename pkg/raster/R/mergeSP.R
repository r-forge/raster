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
	if (!all.x) {
		y$donotusethisvariablename679 <- 1
	}
	d <- merge(d, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, incomprables=incomprable, all.x=all.x, all.y=FALSE)
	d <- d[order(d$donotusethisvariablename976), ]
	d <- d[, -which(colnames(d)=='donotusethisvariablename976')]
	x@data <- d
	if (! all.x ) {
		x <- x[!is.na(x@data$donotusethisvariablename679), ] 
		x@data <- x@data[ , -which(colnames(x@data)=='donotusethisvariablename679')]
	}
	x
} )


