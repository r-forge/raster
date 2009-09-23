# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : March 2009
# Version 0.8
# Licence GPL v3

if (!isGeneric("unstack")) {
	setGeneric("unstack", function(x, ...)
		standardGeneric("unstack"))
}	


setMethod("unstack", signature(x='RasterStack'), 
function(x) {
	return(x@layers)
} )


setMethod("unstack", signature(x='RasterBrick'), 
function(x) {
	rlist <- list()
	for (i in seq(along=nlayers(x))) {
		rlist <- c(rlist, raster(x, i))
	}
	return(rlist)
} )

