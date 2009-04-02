# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3



stackFromFiles <- function(rasterfiles, bands= rep(1, length(rasterfiles))) {
#	stop("this function is depracated. Use makeStack() instead.")
	rstack <- new("RasterStack") 
	return(addFiles(rstack, rasterfiles, bands))
}



if (!isGeneric("stack")) {
	setGeneric("stack", function(x, ...)
		standardGeneric("stack"))
}	

setMethod("stack", signature(x='Raster'), 
function(x, ..., bands=NULL) {
	rlist <- c(x, list(...))
	return(stack(rlist, bands))	
} )



setMethod("stack", signature(x='character'), 
function(x, ..., bands=NULL) {
	rlist <- c(x, list(...))
	return(stack(rlist, bands))
} )


setMethod("stack", signature(x='list'), 
function(x, bands=NULL) {
	j <- 0
	r <- list()
	for (i in 1:length(x)) {
		j <- j + 1
		if (is.character(x[[i]])) {
			if (is.null(bands)) {
				r[j] <- raster(x[[i]])
			} else {
				if (bands[[i]] < 1) {
					r[j] <- raster(x[[i]], 1)
					bds <- nbands(r)
					if (bds > 1) {
						for (b in 2:bds) {
							j <- j + 1
							r[j] <- raster(x[[i]], b)
						}
					}
				}
			}
		} else if (extends(class(x[[i]]), "Raster")) {
			r[j] <- x[[i]]
		} else {
			stop("Arguments should be Raster* objects or filenames")
		}	
	}
	return(addLayer(new("RasterStack"), r))
} )


