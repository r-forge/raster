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
	rlist <- list()
	for (i in nlayers(x):1) {
		rlist[i] <- raster(x, i)
		x <- dropLayer(x, i)
	}
	return(rlist)
} )

