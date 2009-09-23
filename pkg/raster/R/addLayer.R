# R package 'raster'
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("addLayer")) {
	setGeneric("addLayer", function(x, ...)
		standardGeneric("addLayer"))
}	




setMethod('addLayer', signature(x='RasterStack'), 
function(x, ...) {
#x is a list of r objects
	
	rasters <- .makeRasterList(...)
	
	for (i in seq(along=rasters)) { 
		r <- rasters[[i]]
		if (nlayers(x) == 0) {
			x@nrows <- nrow(r)
			x@ncols <- ncol(r)
			x@extent <- extent(r)
			projection(x) <- projection(r)

			if (dataSource(r) == 'ram' & dataContent(r) != 'all') {
				stop('Cannot add a RasterLayer with no associated data in memory or on disk to a RasterStack')
			} else {
				nl <- 1
					if (trim(r@file@shortname) != "") {
					cname <- trim(r@file@shortname)
				} else {
					cname <- "layer1"
				}
				x@layernames[1] <- cname
				x@layers[nl] <- r 
			}
		} else {
			if (!compare(c(x, r))) { 
				stop(paste("could not add r:", filename(r))) 
			}
			if (dataSource(r) == 'ram') {
				if (dataContent(r) != 'all') { 
					stop('Cannot add a RasterLayer with no associated data in memory or on disk to a RasterStack')
				}
			}
			nl <- nlayers(x) + 1 
			count <- 1
			cname <- trim(r@file@shortname)
			if (cname == "") {
				cname <- paste("layer", nl, sep="")
			}
			cn <- cname
			for (j in 1:(nl-1)) {
				if ( cn == layerNames(x)[j] ) { 
					count <- count + 1 
					cn <- paste(cname, "_", count, sep="")
				}
			}	
			x@layernames[nl] <- cn
			
			if (dataSource(r) == 'disk') {
					r <- clearValues(r)
			}
			x@layers[nl] <- r 
		}	
	}
	return(x)
}	
)




setMethod('addLayer', signature(x='RasterBrick'), 
function(x, ...) {
#x is a list of r objects

	rasters <- .makeRasterList(...)
	
	for (i in 1:length(x)) { 
		r <- rasters[[i]]

		if (nlayers(x) == 0) {
			x@nrows <- nrow(r)
			x@ncols <- ncol(r)
			x@extent <- extent(r)
			projection(x) <- projection(r)
			if (dataSource(r) == 'ram' & dataContent(r) != 'all') {
				stop('Cannot add a RasterLayer with no associated data in memory or on disk to a RasterBrick')
			} 
			nl <- 1
			if (trim(r@file@shortname) != "") {
				cname <- trim(r@file@shortname)
			} else {
				cname <- "layer1"
			}
			x@data@colnames <- cname
			x@data@values <- as.matrix(getValues(r))
			x@data@nlayers <- as.integer(1)
			x@data@content <- 'all'
			
		} else {
			if (!compare(c(x, r))) { 
				stop(paste("could not add r:", filename(r))) 
			}
				
			if (dataSource(r) == 'ram') {
				if (dataContent(r) != 'all') { 
					stop('Cannot add a RasterLayer with no associated data in memory or on disk to a RasterStack')
				}
			}

			x@data@values <- cbind(x@data@values, getValues(r))
				
			nl <- nlayers(x) + 1 
			count <- 1
			cname <- trim(r@file@shortname)
			if (cname == "") {
				cname <- paste("layer", nl, sep="")
			}
			cn <- cname
			for (j in 1:(nl-1)) {
				if ( cn == layerNames(x)[j] ) { 
				count <- count + 1 
				cn <- paste(cname, "_", count, sep="")
				}
			}	
			x@data@colnames[nl] <- cn
			x@data@nlayers <- as.integer(nl)

		}
	}	
	return(x)
}	
)

