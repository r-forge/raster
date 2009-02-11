# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



clearValues <- function(object) {
	if (class(object) == "BasicRaster") {
		return(object)
	}
	object@data@content <- 'nodata'
	object@data@indices = vector(mode='numeric')
	if (class(object) == 'RasterStack') {
# need to check if each raster has data on disk. Other wise should not be able to clear	
		object@data@values <- matrix(NA,0,0)
	} else {
		object@data@values <- vector()
		if (dataSource(object) == 'ram') {
			object@data@min <- Inf
			object@data@max <- -Inf	
			object@data@haveminmax <- FALSE
		}
	}
	return(object)
}



