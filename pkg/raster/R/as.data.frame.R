# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


.insertColsInDF <- function(x, y, col, combinenames=TRUE) {
	cnames <- NULL
	if (combinenames) {
		if (ncol(y) > 1) {
			cnames <- paste(colnames(x)[col], '_', colnames(y), sep='')
		}
	}
	if (ncol(y) == 1) {
		x[, col] <- y
		return(x)
	} else if (col==1) {
		z <- cbind(y, x[, -1, drop=FALSE])
	} else if (col==ncol(x)) {
		z <- cbind(x[, -ncol(x), drop=FALSE], y)
	} else {
		z <- cbind(x[,1:(col-1), drop=FALSE], y, x[,(col+1):ncol(x), drop=FALSE])
	}
	if (!is.null(cnames)) {
		colnames(z)[col:(col+ncol(y)-1)] <- cnames
	}
	z
}

setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, ...) {

		v <- as.data.frame(values(x), row.names=row.names, optional=optional, ...)
		colnames(v) <- names(x)  # for nlayers = 1
		
		i <- is.factor(x)
		if (any(is.factor(x))) {
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], 1))
			} else {
				v <- .insertFacts(x, v, 1:nlayers(x))
			}
		}
	
		v
	}
)



setMethod('as.data.frame', signature(x='SpatialPolygons'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, centroids=TRUE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		}		
		if (centroids) {
			xy <- coordinates(x)
			xy <- cbind(1:nrow(xy), xy)
			colnames(xy) <- c('object', 'x', 'y')
			xy <- as.data.frame(xy, row.names=row.names, optional=optional, ...)
			if (.hasSlot(x, 'data')) {
				return( cbind(xy, x@data ) )
			} else {
				return(xy)
			}
		}
		
		nobs <- length(p@polygons)
		objlist <- list()
		cnt <- 0
		for (i in 1:nobs) {
			nsubobs <- length(p@polygons[[i]]@Polygons)
			ps <- lapply(1:nsubobs, function(x) cbind(x, x+cnt, p@polygons[[i]]@Polygons[[x]]@hole, p@polygons[[i]]@Polygons[[x]]@coords))
			objlist[[i]] <- cbind(i, do.call(rbind, ps))
			cnt <- cnt+nsubobs
		}
		
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'partcum', 'hole', 'x', 'y')
		
		obs <- as.data.frame(obs, row.names=row.names, optional=optional, ...)
		
		if (.hasSlot(x, 'data')) {
			d <- x@data
			d <- data.frame(object=1:nrow(x), x@data)
			return( merge(obs, d, by=1) )
		} 
		return( obs )
	}
)



setMethod('as.data.frame', signature(x='SpatialLines'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		}
				
		nobj <- length(p@lines)
		objlist <- list()
		cnt <- 0
		for (i in 1:nobj) {
			nsubobj <- length(p@line[[i]]@Lines)
			ps <- lapply(1:nsubobj, function(x) cbind(x, x+cnt, p@lines[[i]]@Lines[[x]]@coords))
			objlist[[i]] <- cbind(i, do.call(rbind, ps))
			cnt <- cnt+nsubobj
		}
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'partcum', 'x', 'y')
		obs <- as.data.frame(obs, row.names=row.names, optional=optional, ...)
		
		if (.hasSlot(x, 'data')) {
			d <- x@data
			d <- data.frame(object=1:nrow(x), x@data)
			obs <- merge(obs, d, by=1)
		} 
		return (obs)
	}
)




setMethod('as.data.frame', signature(x='SpatialPoints'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		} else {
			xy <- coordinates(x)
			xy <- cbind(1:nrow(xy), xy)
			colnames(xy) <- c('object', 'x', 'y')
			xy <- as.data.frame(xy, row.names=row.names, optional=optional, ...)
			if (.hasSlot(x, 'data')) {
				xy <- data.frame(xy, x@data )
			} 
			return(xy)
		}
	}
)
		
