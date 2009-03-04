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

setMethod("stack", signature(x='RasterLayer'), 
function(x, ...) {
	rlist <- c(x, list(...))
	return(stack(rlist))	
} )


setMethod("stack", signature(x='character'), 
function(x, ...) {
	rlist <- c(x, list(...))
	return(stack(rlist))
} )


setMethod("stack", signature(x='list'), 
function(x) {
	for (i in 1:length(x)) {
		if (is.character(x[[i]])) {
			x[i] <- rasterFromFile(x[[i]])
		} else {
			if (class(x[[i]]) != "RasterLayer") {
				stop("Arguments should be RasterLayer objects or filenames")
			}
		}	
	}
	return(addRasters(new("RasterStack"), x))
} )


