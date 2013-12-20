# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("cover")) {
	setGeneric("cover", function(x, y, ...)
		standardGeneric("cover"))
}	

setMethod('cover', signature(x='SpatialPolygons', y='SpatialPolygons'), 
	function(x, y, ..., identity=FALSE){ 
	
	yy <- list(y, ...)

	i <- which(sapply(yy, function(x) inherits(x, 'SpatialPolygons')))
	if (length(i)==0) {
		stop('additional arguments should be of class SpatialPolygons')
	} else if (length(i) < length(yy)) {
		warning('additional arguments that are not of class SpatialPolygons are ignored')
		yy <- yy[i]
	} 

	if (identity) {
		return(.coverIdentity(x, yy))
	}
	
	haswarned <- FALSE
	for (y in yy) {
		if (! identical(proj4string(x), proj4string(y)) ) {
			if (!haswarned) {
				warning('non identical CRS')
				haswarned <- TRUE
			}
			y@proj4string <- x@proj4string
		}	
		subs <- gIntersects(x, y, byid=TRUE)
		if (!any(subs)) {
			next
		} else {
			int <- .cropSpatPolys(y, x)
			x <- erase(x, int)
			x <- combine(x, int)
		}
	}
	x
} 
)


..cropSpatPolys <- function(x, y, ...) {
	
		y <- gUnaryUnion(y)
		row.names(y) <- '1'
		rnx <- row.names(x)
		row.names(x) <- as.character(1:length(rnx))
		
		if (.hasSlot(x, 'data')) {
			
			# to keep the correct IDs
			# in future versions of rgeos, this intermediate step won't be necessary
			i <- as.vector( gIntersects(x, y, byid=TRUE) )
			if (sum(i) == 0) {
				return(NULL)
			}
			y <- gIntersection(x[i,], y, byid=TRUE)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			if (is.null(y)) { return(y) }
			
			ids <- strsplit(row.names(y), ' ') 
			ids <- as.numeric(do.call(rbind, ids)[,1])
			row.names(y) <- as.character(rnx[ids])
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- rnx[ids]
			
			return( SpatialPolygonsDataFrame(y, data) )
		} else {
			y <- gIntersection(x, y)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			return(y)
		}
}



.coverIdentity <- function(x, yy) {

	haswarned <- FALSE
	for (y in yy) {
		if (! identical(proj4string(x), proj4string(y)) ) {
			if (!haswarned) {
				warning('non identical CRS')
				haswarned <- TRUE
			}
			y@proj4string <- x@proj4string
		}	
		
		i <- gIntersects(x, y)
		if (!i) {
			next
		}
	
		x <- spChFIDs(x, as.character(1:length(row.names(x))))
		y <- spChFIDs(y, as.character(1:length(row.names(y))))

		xnames <- colnames(x@data)
		ynames <- colnames(y@data)
		yinx <- which(ynames %in% xnames)
		doAtt <- TRUE
		if (length(yinx) == 0) {
			doAtt <- FALSE
		}
		
		subs <- gIntersects(x, y, byid=TRUE)

		subsx <- apply(subs, 2, any)
		subsy <- apply(subs, 1, any)
	
		int  <- gIntersection(x[subsx,], y[subsy,], byid=TRUE)
		if (inherits(int, "SpatialCollections")) {
			if (is.null(int@polyobj)) { # ??
				warning('polygons do not intersect')
				next
			}
			int <- int@polyobj
		}
		if (!inherits(int, 'SpatialPolygons')) {
			warning('polygons do not intersect')
			next
		}

		if (doAtt) {
			ids <- do.call(rbind, strsplit(row.names(int), ' '))
			idsy <- match(ids[,2], rownames(y@data))
			rows <- 1:length(idsy)
			
			dat <- x@data[NULL, ,drop=FALSE]
			dat[rows, yinx] <- y@data[idsy, yinx]
			rownames(dat) <- 1:nrow(dat)
			int <- SpatialPolygonsDataFrame(int, dat)
		}
		x <- x - int
		if (is.null(x)) {
			x <- int
		} else {
			x <- append(x, int)
		}
	}
	x
} 




