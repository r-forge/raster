# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



#setMethod('c', signature(x='Raster'), 
#	function (x, ..., recursive=FALSE)  {
#		if (recursive) { warning('argument "recursive=TRUE" is ignored') }
#		merge(x, ...)
#	}
#)


setMethod('c', signature(x='SpatialPolygons'), 
	function (x, ..., recursive=FALSE)  {
		if (recursive) { warning('argument "recursive=TRUE" is ignored') }
		
		x <- list(x, ...)
		rwn <- lapply(x, row.names)
		ln <- sapply(rwn, length)
		rnu <- raster:::.uniqueNames(unlist(rwn))
		end <- cumsum(ln)
		start <- c(0, end[-length(end)]) + 1
		for (i in 1:length(x)) {
			if (! all(rnu[start[i]:end[i]] == rwn[[i]]) ) {
				row.names(x[[i]]) <- rnu[start[i]:end[i]]
			}
		}
		
		dat <- NULL
		dataFound <- FALSE
		for (i in 1:length(x)) {
			if (.hasSlot(x[[i]], 'data')) {
				dataFound <- TRUE
				if (is.null(dat)) {
					dat <- x[[i]]@data
				} else {
					d <- x[[i]]@data
					z <- which(!colnames(dat) %in% colnames(d))
					if (length(z) > 1) {
						dd <- dat[NULL, z]
						dd[1:nrow(d),] <- NA
						d <- cbind(d, dd)
					}
					z <- which(!colnames(d) %in% colnames(dat))
					if (length(z) > 1) {
						dd <- d[NULL, z]
						dd[1:nrow(dat),] <- NA
						dat <- cbind(dat, dd)
					}
					dat <- rbind(dat, d)
				}
			} else {
				if ( is.null(dat)) {
					dat <- data.frame()
					dat[1:length(x[[i]]@polygons),] <- NA
				} else {
					dd <- dat[NULL, ]
					dd[1:length(x[[i]]@polygons),] <- NA
					dat <- rbind(dat, dd)
				}	
			}
		}
		if (! dataFound ) {
			return( do.call(rbind, x) )
		}
		x <- sapply(x, function(x) as(x, 'SpatialPolygons'))
		x <- do.call(rbind, x)
		rownames(dat) <- row.names(x)
		SpatialPolygonsDataFrame(x, dat)
	}
)

