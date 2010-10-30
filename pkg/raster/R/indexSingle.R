# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[", c("Raster", "Spatial", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (inherits(i, 'SpatialGrid') | inherits(i, 'SpatialPixels')) {
		i <-  as(i, 'SpatialPoints')
	}
	extract(x, i, ...)
})


setMethod("[", c("Raster","ANY", "missing"),
function(x,i,j,...,drop=TRUE) {
	
	if (! hasValues(x) ) {
		stop('no data associated with this RasterLayer object')
	}
	
	if (missing(i)) {
		return(getValues(x))
	}

	if (inherits(i, "RasterLayer")) {
		i <- as.logical( getValues(i) ) 
	}
	
	return( .cellValues(x, i) )
}
)




