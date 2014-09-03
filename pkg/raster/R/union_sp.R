# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("union")) {
	setGeneric("union", function(x, y)
		standardGeneric("union"))
}	


setMethod('union', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	stopifnot(require(rgeos))

	x <- spChFIDs(x, as.character(1:length(row.names(x))))
	y <- spChFIDs(y, as.character(1:length(row.names(y))))

	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}
	
	subs <- gIntersects(x, y, byid=TRUE)
	
	if (!any(subs)) {
	
		x <- bind(x, y)
		
	} else {
	
		xdata <- .hasSlot(x, 'data')
		ydata <- .hasSlot(y, 'data')
		if (xdata & ydata) {
			nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
			colnames(x@data) <- nms[1:ncol(x@data)]
			colnames(y@data) <- nms[(ncol(x@data)+1):length(nms)]
		} 
		
		dif1 <- erase(x, y)
		dif2 <- erase(y, x)
		
		x <- intersect(x, y)
		x <- list(dif1, dif2, x)
		x <- x[!sapply(x, is.null)]
		i <- sapply(x, length) # 
		x <- x[ i > 0]
		if (length(x) > 1) {
			x <- do.call(bind, x)
		} else {
			x <- x[[1]]
		}
		
		# remove slivers
		area <- sapply(x@polygons, function(i) slot(i, 'area'))
		x <- x[area > 0, ]
	}

	x
}
)



.simpleUnion <- function(x, y) {
	subs <- gIntersects(x, y)
	if (!any(subs)) {
		x@polygons <- c(x@polygons, y@polygons)
		x <- spChFIDs(x, as.character(1:length(x)))
	} else {
		dif1 <- erase(x, y)
		dif2 <- erase(y, x)
		x <- intersect(x, y)
		x <- list(dif1, dif2, x)
		x <- x[!sapply(x, is.null)]
		i <- sapply(x, length) # 
		x <- x[ i > 0]
		y <- x[[1]]
		if (length(x) == 2) {
			y@polygons <- c(y@polygons, x[[2]]@polygons)
		} else if (length(x) == 3) {
			y@polygons <- c(y@polygons, x[[2]]@polygons, x[[3]]@polygons)		
		}
		# remove slivers
		y <- spChFIDs(y, as.character(1:length(y)))
		area <- sapply(y@polygons, function(i) slot(i, 'area'))
		x <- y[area > 0, ]
	}
	
	x
}





setMethod('union', signature(x='SpatialPolygons', y='missing'), 
function(x, y) {
	stopifnot(require(rgeos))
	n <- length(x)
	if (n < 2) {
		return(x)
	}
	if (.hasSlot(x, 'data')) {
		x <- as(x, 'SpatialPolygons')
		#x$ID <- 1:n
		#x@data <- x@data[, 'ID', drop=FALSE]
	} else {
		#x <- SpatialPolygonsDataFrame(x, data.frame(ID=1:n))
	}
	
	a <- gArea(s, byid=T)
	ord <- order(a)
	u <- x[ord[1],]
	cnt = 1
#	names(u) <- 'p1'
	r <- list()
	ri <- 1
	for (i in ord[-1]) {
		cnt <- cnt + 1
		if (cnt %% 50 == 0) {
			r[i] <- u
			i <- i + 1
			u <- x[i,]
		} else {
			u <- .simpleUnion(u, x[i, ])
		}
#		names(u)[i] <- paste('p', i, sep='')
		#cat(paste('p', cnt, '\n', sep=''))
		#flush.console()
		#uu <<- u
	}
	
	if (length(ri) == 1) {
		return(ri[[1]])
	} else {
		u <- ri[[1]]
		for (i in 2:length(ri)) {
			u <- .simpleUnion(u, ri[i, ])
		}
		return(u)
	}
	
	#u@data[!is.na(u@data)] <- 1
	#u@data[is.na(u@data)] <- 0
	#u$count <- rowSums(u@data)
	
	#xy <- SpatialPoints(coordinates(u), proj4string=crs(u))
	#u <- SpatialPolygonsDataFrame(u, data.frame(poly.ID=1:length(u)))
	#e <- extract(xy, x)
	
	#e <- reshape(e    )  xxx
	#u <- merge(u, e, by='poly.ID')
	
	u
}	
)


