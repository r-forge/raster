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



setMethod('merge', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y, ...) {

	stop('work in progress, not working yet')
	
	dat <- NULL
	xdata <- ydata <- FALSE
	if (.hasSlot(x, 'data')) {
		xdata <- TRUE
		dat <- x@data[1, ,drop=FALSE]
		rownames(dat) <- NULL
	} 
	if (.hasSlot(y, 'data')) {
		ydata <- TRUE
		ydat <- y@data[1, ,drop=FALSE]
		rownames(ydat) <- NULL
		if (is.null(dat)) {
			dat <- ydat
		} else {
			cx <- colnames(dat)
			xy <- colnames(ydat)
			for (i in ncol(ydat):1) {
				if (xy[i] %in% cx) {
					# should also check for datatype
					xy <- xy[-i]
				}
			}
			if (length(xy) > 0) {
				nc <- ncol(dat)
				dat <- cbind(dat, ydat[,xy])
				colnames(dat)[(nc+1):(nc+length(xy))] <- xy
			}
		}
	}
	if (!is.null(dat)) {
		dat <- dat[-1,]
	}

	res <- NULL
	dif1 <- gDifference(x, y, byid=TRUE)
	if (!is.null(dif1)) {
		res <- dif1@polygons
		if (xdata) {
			ids <- sapply(dif1@polygons, function(y) slot(y, 'ID'))
			ids <- as.numeric(sapply(ids, function(x) strsplit(x, ' ')[[1]][1]))
		# which rownames not correct
			ids <- which(rownames(x@data)==ids)
			dat[1:length(ids),colnames(x@data)] <- x@data[ids,]
		}
	}
	dif2 <- gDifference(y, x, byid=TRUE)
	if (!is.null(dif2)) {
		res <- c(res, dif2@polygons)
		if (ydata) {
			ids <- sapply(dif2@polygons, function(y) slot(y, 'ID'))
			ids <- sapply(ids, function(x) strsplit(x, ' ')[[1]][1])
		# which rownames not correct
			ids <- which(rownames(y@data)==ids)
			dat[(nrow(dat)+1):(nrow(dat)+length(ids)),colnames(y@data)] <- y@data[ids,]
		}
	}
	int  <- gIntersection(x, y, byid=TRUE)
	if (!is.null(int)) {
		res <- c(res, int@polygons)
		ids <- sapply(int@polygons, function(y) slot(y, 'ID'))
		ids <- sapply(ids, function(x) strsplit(x, ' ')[[1]])
		rows <- (nrow(dat)+1):(nrow(dat)+length(ids))
		if (xdata) {
			# which rownames does not do it
			idsx <- which(rownames(x@data)==ids[1,])
			dat[rows, colnames(x@data)] <- x@data[idsx,]
		} else if (ydata) {
			idsy <- which(rownames(y@data)==ids[2,])
			dat[rows, colnames(y@data)] <- y@data[idsy,]
		}
	}
	
	res <- SpatialPolygons(lapply(1:length(res), function(y) Polygons(res[[y]]@Polygons, y)))
	if (!is.null(dat)) {
		res <- SpatialPolygonsDataFrame(res, dat)
	}
	res
}
)



