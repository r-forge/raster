# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0,7
# Licence GPL v3


if (!isGeneric("nlayers")) {
	setGeneric("nlayers", function(object)
		standardGeneric("nlayers"))
}	

setMethod('nlayers', signature(object='BasicRaster'), 
	function(object){
		return(0) 
    }
)

setMethod('nlayers', signature(object='Raster'), 
	function(object){
		return(1) 
    }
)

setMethod('nlayers', signature(object='RasterStack'), 
	function(object){
		return(object@data@nlayers) 
    }
)

setMethod('nlayers', signature(object='Spatial'), 
	function(object){
		if ( class(object)=='SpatialPixelsDataFrame' |  class(object)=='SpatialGridDataFrame' ) { 
			return( dim(object@data)[2] ) 
		} else {
			return( 0 )
		}
    }
)


layerNames <- function(object) {
	if (class(object) == "RasterLayer") {
		return(filename(object))
	} else if (class(object) == "RasterStack") {
		l <- vector('character')
		for (i in 1:nlayers(object)) {
			l <- c(l, filename(asRasterLayer(object, i)))
		}
		return(l)
	}	
}
